import { LLType, parType, tensorType } from "./lltype";

export type ContextId = number;

export type ContextBranchType = "tensor" | "par";

export type ContextLeafNode = {
  nodeType: "leaf";
  id: ContextId;
  parent: ContextId;
  type: LLType;
};

export type ContextBranchNode = {
  nodeType: "branch";
  id: ContextId;
  parent?: ContextId;
  children: ContextId[];
  branchType: ContextBranchType;
};

export type ContextNode = ContextLeafNode | ContextBranchNode;

export type ContextTree = {
  structure: Map<ContextId, ContextNode>;
  root: ContextId;
  nextId: ContextId;
};

export class ContextTreeError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ContextTreeError";
  }
}

export class ContextTreeErrorNodeNotFound extends ContextTreeError {
  constructor(id: ContextId) {
    super(`Node with id ${id} not found`);
  }
}

export class ContextTreeErrorCannotConnect extends ContextTreeError {
  constructor(id1: ContextId, id2: ContextId) {
    super(`Node with id ${id1} cannot be connected to node with id ${id2}`);
  }
}

export class ContextTreeErrorInvalidNodeType extends ContextTreeError {
  constructor(parent: ContextId, expectedType: string, actualType?: string) {
    super(
      `Node with id ${parent} is not a ${expectedType}, it is a ${actualType}`
    );
  }
}

export class ContextTreeErrorInvalidBranchType extends ContextTreeError {
  constructor(
    parent: ContextId,
    expectedType: ContextBranchType,
    actualType?: ContextBranchType
  ) {
    super(
      `Node with id ${parent} is not a ${expectedType}, it is a ${actualType}`
    );
  }
}

export function createContextTree(): ContextTree {
  return {
    structure: new Map(),
    root: 0,
    nextId: 0,
  };
}

export function copyContextTree(tree: ContextTree): ContextTree {
  return {
    structure: new Map(
      tree.structure.entries().map(([key, value]) => [
        key,
        value.nodeType === "leaf"
          ? {
              nodeType: "leaf",
              id: value.id,
              parent: value.parent,
              type: value.type,
            }
          : {
              nodeType: "branch",
              id: value.id,
              parent: value.parent,
              children: [...value.children],
              branchType: value.branchType,
            },
      ])
    ),
    root: tree.root,
    nextId: tree.nextId,
  };
}

export function getNode(tree: ContextTree, id: ContextId): ContextNode {
  const node = tree.structure.get(id);
  if (!node) {
    throw new ContextTreeErrorNodeNotFound(id);
  }
  return node;
}

export function getBranchNode(
  tree: ContextTree,
  id: ContextId
): ContextBranchNode {
  const node = getNode(tree, id);
  if (node.nodeType !== "branch") {
    throw new ContextTreeErrorInvalidNodeType(id, "branch", node.nodeType);
  }
  return node;
}

export function getLeafNode(tree: ContextTree, id: ContextId): ContextLeafNode {
  const node = getNode(tree, id);
  if (node.nodeType !== "leaf") {
    throw new ContextTreeErrorInvalidNodeType(id, "leaf", node.nodeType);
  }
  return node;
}

export function getParentNode(
  tree: ContextTree,
  id: ContextId
): ContextBranchNode | undefined {
  const node = getNode(tree, id);
  return node.parent ? getBranchNode(tree, node.parent) : undefined;
}

export function getNewId(tree: ContextTree): ContextId {
  return tree.nextId++;
}

export function addLeafNode(
  tree: ContextTree,
  parent: ContextId,
  type: LLType
): ContextId {
  const parentNode = getBranchNode(tree, parent);
  const id = getNewId(tree);
  tree.structure.set(id, {
    nodeType: "leaf",
    id,
    parent,
    type,
  });
  parentNode.children.push(id);
  return id;
}

export function addBranchNode(
  tree: ContextTree,
  parent: ContextId,
  branchType: ContextBranchType
): ContextId {
  const parentNode = getBranchNode(tree, parent);
  const id = getNewId(tree);
  tree.structure.set(id, {
    nodeType: "branch",
    id,
    parent,
    children: [],
    branchType,
  });
  parentNode.children.push(id);
  return id;
}

export function moveNode(
  tree: ContextTree,
  targetNodeId: ContextId,
  parent: ContextId
) {
  const targetNode = getNode(tree, targetNodeId);
  const oldParentNode = getParentNode(tree, targetNodeId);
  const newParentNode = getBranchNode(tree, parent);

  if (oldParentNode) {
    oldParentNode.children = oldParentNode.children.filter(
      (id) => id !== targetNodeId
    );
  }

  newParentNode.children.push(targetNodeId);
  targetNode.parent = parent;
}

export function removeNode(tree: ContextTree, nodeId: ContextId) {
  getNode(tree, nodeId);
  const parentNode = getParentNode(tree, nodeId);
  if (parentNode) {
    parentNode.children = parentNode.children.filter((id) => id !== nodeId);
  }
  tree.structure.delete(nodeId);
}

export function addRootNode(tree: ContextTree, branchType: ContextBranchType) {
  const currentRoot = getNode(tree, tree.root);
  const id = getNewId(tree);
  tree.structure.set(id, {
    nodeType: "branch",
    id,
    parent: undefined,
    children: [tree.root],
    branchType,
  });
  currentRoot.parent = id;
  tree.root = id;
  return id;
}

/**
 * Insert node before id
 */
export function insertBranchNode(
  tree: ContextTree,
  targetId: ContextId,
  branchType: ContextBranchType
): ContextId {
  const parentNode = getParentNode(tree, targetId);
  if (!parentNode) {
    throw new ContextTreeErrorNodeNotFound(targetId);
  }
  const newNode = addBranchNode(tree, parentNode.id, branchType);
  moveNode(tree, targetId, newNode);
  return newNode;
}

export function destructNode(
  tree: ContextTree,
  id: ContextId,
  branchType: ContextBranchType
): ContextId[] {
  const node = getNode(tree, id);
  if (node.nodeType !== "leaf") {
    throw new ContextTreeErrorInvalidNodeType(id, "leaf", node.nodeType);
  }
  const llType = node.type;
  if (llType.type !== branchType) {
    throw new ContextTreeErrorInvalidBranchType(id, branchType);
  }
  const parentNode = getParentNode(tree, id);

  if (!parentNode) {
    throw new ContextTreeErrorNodeNotFound(id);
  }

  removeNode(tree, id);
  const newParentId = addBranchNode(tree, parentNode.id, branchType);
  return llType.elements.map((element) => {
    return addLeafNode(tree, newParentId, element);
  });
}

export function leastCommonAncestor(
  tree: ContextTree,
  id1: ContextId,
  id2: ContextId
): {
  path1: ContextId[]; // Not including id1, starting from lca
  path2: ContextId[]; // Not including id2, starting from lca
  lca: ContextId;
} {
  return {} as any;
}

export function connectNode(
  tree: ContextTree,
  id1: ContextId,
  id2: ContextId,
  branchType: ContextBranchType
): ContextId {
  const { path1, path2, lca } = leastCommonAncestor(tree, id1, id2);
  const lcaNode = getBranchNode(tree, lca);

  if (branchType === "par") {
    if (lcaNode.branchType !== "par") {
      throw new ContextTreeErrorInvalidBranchType(
        lca,
        "par",
        lcaNode.branchType
      );
    }
    // Every node in path1 and path2 should be a "par"
    path1.forEach((id) => {
      const node = getBranchNode(tree, id);
      if (node.branchType !== "par") {
        throw new ContextTreeErrorInvalidBranchType(id, "par", node.branchType);
      }
    });
    path2.forEach((id) => {
      const node = getBranchNode(tree, id);
      if (node.branchType !== "par") {
        throw new ContextTreeErrorInvalidBranchType(id, "par", node.branchType);
      }
    });
    // Then, we can remove id1, id2 and add (id1 `par` id2) type to lca
    removeNode(tree, id1);
    removeNode(tree, id2);
    const newId = addLeafNode(
      tree,
      lca,
      parType([getLeafNode(tree, id1).type, getLeafNode(tree, id2).type])
    );
    return newId;
  } else {
    if (lcaNode.branchType !== "tensor") {
      throw new ContextTreeErrorInvalidBranchType(
        lca,
        "tensor",
        lcaNode.branchType
      );
    }
    // Recursivelly merge path1 and path2
    function mergeBranch(
      remainingPath1: ContextId[], // not include parent
      remainingPath2: ContextId[],
      parent: ContextId
    ): ContextId {
      if (remainingPath1.length === 0 && remainingPath2.length === 0) {
        // Delete id1, id2, then add (id1 `tensor` id2) to parent
        removeNode(tree, id1);
        removeNode(tree, id2);
        const newId = addLeafNode(
          tree,
          parent,
          tensorType([getLeafNode(tree, id1).type, getLeafNode(tree, id2).type])
        );
        return newId;
      }
      if (remainingPath1.length === 0) {
        const node2 = getBranchNode(tree, remainingPath2[0]);
        const branchType = node2.branchType;
        // Insert a singleton node before node id1 (branchType)
        const singletonId = insertBranchNode(tree, id1, branchType);
        // Then add this node to remainingPath1
        const newRemainingPath1 = [singletonId];
        return mergeBranch(newRemainingPath1, remainingPath2, parent);
      }
      if (remainingPath2.length === 0) {
        const node1 = getBranchNode(tree, remainingPath1[0]);
        const branchType = node1.branchType;
        // Insert a singleton node before node id2 (branchType)
        const singletonId = insertBranchNode(tree, id2, branchType);
        // Then add this node to remainingPath2
        const newRemainingPath2 = [singletonId];
        return mergeBranch(remainingPath1, newRemainingPath2, parent);
      }
      const node1 = getBranchNode(tree, remainingPath1[0]);
      const node2 = getBranchNode(tree, remainingPath2[0]);
      if (node1.branchType === "tensor" && node2.branchType === "par") {
        const singletonId = insertBranchNode(tree, remainingPath2[0], "tensor");
        const newRemainingPath2 = [singletonId, ...remainingPath2.slice(1)];
        return mergeBranch(remainingPath1, newRemainingPath2, parent);
      }
      if (node1.branchType === "par" && node2.branchType === "tensor") {
        const singletonId = insertBranchNode(tree, remainingPath1[0], "tensor");
        const newRemainingPath1 = [singletonId, ...remainingPath1.slice(1)];
        return mergeBranch(newRemainingPath1, remainingPath2, parent);
      }
      // then node1.branchType === node2.branchType
      const branchType = node1.branchType;
      // Create new node under the parent, then move all children of node1 and node2 to the new node
      // Then, delete old node1 and node2
      // Remove heads of remainingPath1 and remainingPath2, then recursively merge them
      const newParentId = addBranchNode(tree, parent, branchType);
      node1.children.forEach((child) => {
        moveNode(tree, child, newParentId);
      });
      node2.children.forEach((child) => {
        moveNode(tree, child, newParentId);
      });
      removeNode(tree, id1);
      removeNode(tree, id2);
      return mergeBranch(
        remainingPath1.slice(1),
        remainingPath2.slice(1),
        newParentId
      );
    }
    const remainingPath1 = path1.slice(1);
    const remainingPath2 = path2.slice(1);
    return mergeBranch(remainingPath1, remainingPath2, lca);
  }
}
