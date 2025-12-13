import {
  createContextTree,
  normalize,
  getNode,
  addBranchNode,
  addLeafNode,
  connectNodes,
  removeNode,
  ContextTree,
} from "./contextTree";
import { primitiveType } from "./lltype";

// Utils
function printTree(tree: ContextTree) {
  console.log("Root:", tree.root);
  console.log("Nodes:", Array.from(tree.structure.keys()));
}

// Test 1: Simple intro normalization
try {
  console.log("Test 1: Intro Normalization");
  const tree = createContextTree("tensor"); // Root 0
  const parId = addBranchNode(tree, tree.root, "par"); // ID 1
  const t1 = addLeafNode(tree, parId, primitiveType("A", -1));
  const t2 = addLeafNode(tree, parId, primitiveType("A", 1));

  // Root(0) -> Par(1) -> [A, A]
  normalize(tree);
  // Root should be 1. 0 deleted.
  console.log("Test 1 Result:");
  printTree(tree);
  getNode(tree, tree.root); // Should not throw
  console.log("Test 1 Passed");
} catch (e) {
  console.error("Test 1 Failed:", e);
}

// Test 2: Elim Normalization / Connect
try {
  console.log("\nTest 2: Elim/Connect");
  const tree = createContextTree("tensor");
  const id1 = addLeafNode(tree, tree.root, primitiveType("A", 1)); // 1
  const id2 = addLeafNode(tree, tree.root, primitiveType("A", -1)); // 2

  connectNodes(tree, [id1, id2], "tensor");
  // connectNodes logic:
  // LCA of 1 and 2 is Root(0).
  // mergeBranch called.
  // Removes 1 and 2.
  // Adds new leaf (A*A) to Root.
  // Root(0) -> Leaf(3).
  // Normalize: Root(0) has 1 child(3).
  // Root becomes 3. 0 deleted.

  console.log("Test 2 Result:");
  printTree(tree);
  getNode(tree, tree.root);
  console.log("Test 2 Passed");
} catch (e) {
  console.error("Test 2 Failed:", e);
}

// Test 3: The "Node with id 1 not found" specific scenario?
// Maybe removing 0 sets root to 1. But 1 is then removed?
try {
  console.log("\nTest 3: Nested Normalization");
  const tree = createContextTree("tensor"); // 0
  const branch1 = addBranchNode(tree, tree.root, "tensor"); // 1 (Tensor under Tensor)
  const leaf = addLeafNode(tree, branch1, primitiveType("A", 1)); // 2

  // Root(0) -> Branch(1) -> Leaf(2)
  // All tensor.
  // Normalize:
  // 1. Flatten same-type branches?
  // Branch(1) is Tensor. Parent(0) is Tensor.
  // Branch(1) merged into 0.
  // 0 -> [2]. 1 deleted.
  // 2. Flatten 1 child?
  // 0 has 1 child (2).
  // Root = 2. 0 deleted.
  // Final: Root=2.

  normalize(tree);
  console.log("Test 3 Result:");
  printTree(tree);
  getNode(tree, tree.root);
  console.log("Test 3 Passed");
  // Test 4: Merge Bug Repro
  // Create Root(Tensor).
  // Add Par(1) -> [A, B]
  // Add Par(2) -> [C, D]
  // Connect A and C (Tensor).
  // Should merge Par(1) and Par(2) into Par(3).
  // If bug exists, B or D might be orphaned.
  console.log("\nTest 4: Merge Bug Repro");
  const tree4 = createContextTree("tensor");
  const par1 = addBranchNode(tree4, tree4.root, "par");
  const a = addLeafNode(tree4, par1, primitiveType("A", 1));
  const b = addLeafNode(tree4, par1, primitiveType("B", 1));

  const par2 = addBranchNode(tree4, tree4.root, "par");
  const c = addLeafNode(tree4, par2, primitiveType("C", 1));
  const d = addLeafNode(tree4, par2, primitiveType("D", 1));

  // Connect a and c.
  // This should trigger merge of par1 and par2.
  connectNodes(tree4, [a, c], "tensor");

  console.log("Test 4 Result:");
  printTree(tree4);

  // Check if B and D are valid
  const nodeB = getNode(tree4, b);
  console.log("B parent:", nodeB.parent);
  getNode(tree4, nodeB.parent!);

  const nodeD = getNode(tree4, d);
  console.log("D parent:", nodeD.parent);
  getNode(tree4, nodeD.parent!);

  console.log("Test 4 Passed");
} catch (e) {
  console.error("Test 4 Failed:", e);
}

try {
  console.log("\nTest 5: Empty Tree Normalization");
  const tree = createContextTree("tensor");
  // Root 0, empty.
  normalize(tree);

  console.log("Test 5 Result:");
  printTree(tree);
  if (!tree.structure.has(tree.root)) {
    console.error("Test 5 Failed: Root deleted but pointer remains");
    // This confirms the crash condition if getNode(tree.root) is called
    try {
      getNode(tree, tree.root);
    } catch (e) {
      console.log("Confirmed crash on access:", e);
    }
  } else {
    console.log("Test 5 Passed (Root preserved or updated)");
  }
} catch (e) {
  console.error("Test 5 Failed:", e);
}
