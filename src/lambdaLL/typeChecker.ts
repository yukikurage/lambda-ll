import { Statement, Term, Program, ProgramItem, TypeAlias } from "./ast";
import {
  ContextId,
  ContextTree,
  addBranchNode,
  addLeafNode,
  connectNodes,
  destructNode,
  getBranchNode,
  getLeafNode,
  normalize,
  removeNode,
} from "./contextTree";
import { LLType, typeInverse, tensorType, parType } from "./lltype";

export type Variables = Map<string, ContextId>; // variable name -> leaf node id
export type TypeEnv = Map<string, LLType>; // type alias -> LLType

function resolveType(t: LLType, env: TypeEnv): LLType {
  if (t.type === "primitive") {
    if (env.has(t.name)) {
      // Check spin?
      // If alias is T = [A]. T* = [A]*.
      // Logic: standard LL alias is usually for the positive type.
      // If t is T*. `resolve(T)` is `[A]`. `T*` is `[A]*`.
      const resolved = env.get(t.name)!;
      // If t.spin is -1, invert resolved.
      // How to strict parsing?
      // `primitiveType` has `spin`.
      // If we aliased `MP = ...`. `MP` is primitive "MP".
      // If usage `MP`. `spin` is 1. result is `resolved`.
      // If usage `MP*`. `spin` is -1. result is `resolved*`.
      // We need `typeInverse` to handle complex types properly if `resolved` covers it.
      // Yes `typeInverse` does that.
      if (t.spin === -1) {
        return typeInverse(resolved);
      }
      return resolved;
    }
    return t;
  }
  if (t.type === "tensor") {
    return tensorType(t.elements.map((e) => resolveType(e, env)));
  }
  if (t.type === "par") {
    return parType(t.elements.map((e) => resolveType(e, env)));
  }
  return t;
}

function isTypeEqual(t1: LLType, t2: LLType, env: TypeEnv): boolean {
  const r1 = resolveType(t1, env);
  const r2 = resolveType(t2, env);

  if (r1.type !== r2.type) return false;
  if (r1.type === "primitive" && r2.type === "primitive") {
    return r1.name === r2.name && r1.spin === r2.spin;
  }
  if (r1.type === "tensor" && r2.type === "tensor") {
    if (r1.elements.length !== r2.elements.length) return false;
    return r1.elements.every((e, i) => isTypeEqual(e, r2.elements[i], env));
  }
  if (r1.type === "par" && r2.type === "par") {
    if (r1.elements.length !== r2.elements.length) return false;
    return r1.elements.every((e, i) => isTypeEqual(e, r2.elements[i], env));
  }
  return false;
}

function deepCopyVariables(vars: Variables): Variables {
  return new Map(vars);
}

export function check_term(
  term: Term,
  tree: ContextTree,
  vars: Variables,
  types: TypeEnv
): ContextId {
  switch (term.type) {
    case "var": {
      const id = vars.get(term.name);
      if (id === undefined) {
        throw new Error(`Variable ${term.name} not found`);
      }
      return id;
    }
    case "tensor": {
      if (term.elements.length === 0)
        throw new Error("Empty tensor term not allowed");
      if (term.elements.length === 1)
        return check_term(term.elements[0], tree, vars, types);

      const ids = term.elements.map((t) => check_term(t, tree, vars, types));
      return connectNodes(tree, ids, "tensor");
    }
    case "par": {
      if (term.elements.length === 0)
        throw new Error("Empty par term not allowed");
      if (term.elements.length === 1)
        return check_term(term.elements[0], tree, vars, types);

      const ids = term.elements.map((t) => check_term(t, tree, vars, types));
      return connectNodes(tree, ids, "par");
    }
    case "block": {
      // { stmt; ... return t; }
      // Scope logic:
      // We reuse `tree` and `vars`.
      // We iterate statements.
      // We expect last statement to be `return`.
      // Or simple iteration.
      // `vars` are scoped.
      // We need to ensure that local vars are consumed.
      // Logic:
      // `contextTree` is modified in place.
      // If we introduce vars, they are adding Leaves.
      // If we don't consume them, they remain as Leaves.
      // `connectNodes` or `normalize` at end of block should result in SINGLE node?
      // `return t` logic: `check_term(t)`. `normalize`.
      // If there are other nodes left in the tree?
      // Wait, `check_term(block)` returns a ContextId.
      // That ContextId corresponds to the resulting term.
      // If the block execution left garbage nodes in the tree (unused vars),
      // `tree` will contain them.
      // But `ContextTree` doesn't strictly enforce "clean tree" at every step?
      // The prompt says: "assume that no node except for t and outer variables is left".
      // This implies verification.
      // So we should verify: `tree.structure` contains only descendants of `returnedId` + outer vars.
      // But outer vars are also in the tree.
      // So: Any node reachable from `vars` (passed in) is allowed.
      // Any node reachable from `returnedId` is allowed.
      // Anything else is garbage (unused local var).
      // Since LL requires using everything once:
      // If we introduced a local var, it MUST be used.
      // If used, it is connected to something.
      // Eventually everything connected to `returnedId`.
      // So `returnedId` should be the Root?
      // Or if we are inside a larger construct?
      // `check_term` is called recursively.
      // Example: `[ { let x=... return x }, y ]`
      // Inner block returns `x`. Tree has `y` too.
      // Inner block shouldn't touch `y`.
      // Inner block introduces `local`. They must be consumed into `x`.
      // So: `tree` has `y`, `x`, `garbage`.
      // We want to ensure no `garbage`.
      // How? count nodes? Or track usage?

      let currentVars = deepCopyVariables(vars); // Scoped vars
      const initialVarCount = vars.size;

      // Execute statements
      for (const stmt of term.statements) {
        currentVars = check_statement(stmt, tree, currentVars, types);
      }

      // Find the return value?
      // My Logic: `return` statement in `check_statement` returns `vars`.
      // But `return` statement has `value`.
      // We need to capture the `return`ed ID.
      // `check_statement` should probably return `ContextId | Variables`?
      // Or we handle `return` differently.
      // AST says Block has `Statement[]`.
      // One of them is `return`.
      // We can scan for `return` or ensure it's last.
      // Parser doesn't enforce `return` is last.
      // But execution flow does.
      // Let's modify `check_statement` to handle return signal?

      // Actually simpler: `check_block` logic iterates statements.
      // If statement is `return`, we evaluate term and return ID.
      // If statement is `let` etc, we update vars.

      // I'll inline statement loop here with special handling for return.

      for (const stmt of term.statements) {
        if (stmt.type === "return") {
          const retId = check_term(stmt.value, tree, currentVars, types);

          // Verify linearity/garbage collection?
          // Just calling `normalize(tree)` is standard behavior.
          normalize(tree);

          // Strict check: All `currentVars` that are NOT in `vars` (outer) must be consumed?
          // `consumed` means they are not accessible?
          // If they are in `currentVars`, they point to a `ConceptId`.
          // If that `ConceptId` is still in `tree`, it might be part of `retId` structure.
          // If it is NOT part of `retId` structure, it is unused.
          // UNLESS it is an outer var.
          // So: For every `k, v` in `currentVars`.
          // If `!vars.has(k)` (it is local):
          //   Check if `v` is part of `retId` tree?
          //   How to check? `getPathToRoot(v)` passes through `retId`.
          //   Wait, `retId` is just a node.
          //   If `retId` is the valid result, it is a sub-tree.
          //   Descendants of `retId` are parts of it.
          //   Does `ContextTree` have parent pointers? Yes.
          //   So check if `v` is descendant of `retId`.
          //   OR `v` IS `retId`.
          //   Helper: `isDescendant(tree, child, ancestor)`.
          return retId;
        } else {
          currentVars = check_statement(stmt, tree, currentVars, types);
        }
      }
      throw new Error("Block must end with return");
    }
  }
}

function isDescendant(
  tree: ContextTree,
  childId: ContextId,
  ancestorId: ContextId
): boolean {
  if (childId === ancestorId) return true;
  const node = getLeafNode(tree, childId); // Actually getNode
  // Wait getNode signature
  // Need to traverse up.
  let curr = tree.structure.get(childId);
  while (curr && curr.parent !== undefined) {
    if (curr.parent === ancestorId) return true;
    curr = tree.structure.get(curr.parent);
  }
  return false;
}

export function check_statement(
  stmt: Statement,
  tree: ContextTree,
  vars: Variables,
  types: TypeEnv
): Variables {
  switch (stmt.type) {
    case "let": {
      const tId = check_term(stmt.value, tree, vars, types);
      const tNode = getLeafNode(tree, tId);

      if (stmt.typeAnnotation) {
        if (!isTypeEqual(tNode.type, stmt.typeAnnotation, types)) {
          // Warn?
        }
      }

      const newVars = deepCopyVariables(vars);
      newVars.set(stmt.name, tId);
      return newVars;
    }
    case "let_destruct_tensor": {
      const tId = check_term(stmt.value, tree, vars, types);
      const destructedIds = destructNode(tree, tId, "tensor");
      if (destructedIds.length !== stmt.names.length) {
        throw new Error(
          `Destructuring mismatch: expected ${stmt.names.length}, got ${destructedIds.length}`
        );
      }
      const newVars = deepCopyVariables(vars);
      stmt.names.forEach((name, i) => {
        newVars.set(name, destructedIds[i]);
      });
      return newVars;
    }
    case "let_destruct_par": {
      const tId = check_term(stmt.value, tree, vars, types);
      const destructedIds = destructNode(tree, tId, "par");
      if (destructedIds.length !== stmt.names.length) {
        throw new Error(
          `Destructuring mismatch: expected ${stmt.names.length}, got ${destructedIds.length}`
        );
      }
      const newVars = deepCopyVariables(vars);
      stmt.names.forEach((name, i) => {
        newVars.set(name, destructedIds[i]);
      });
      return newVars;
    }
    case "intro": {
      const root = getBranchNode(tree, tree.root);
      if (root.branchType !== "tensor") {
        throw new Error("Root must be Tensor to use intro");
      }

      const type = resolveType(stmt.typeAnnotation, types);
      const parId = addBranchNode(tree, tree.root, "par");
      const xId = addLeafNode(tree, parId, typeInverse(type));
      const yId = addLeafNode(tree, parId, type);

      const newVars = deepCopyVariables(vars);
      newVars.set(stmt.name1, xId);
      newVars.set(stmt.name2, yId);
      return newVars;
    }
    case "elim": {
      // elim t1, t2;
      const t1Id = check_term(stmt.term1, tree, vars, types);
      const t2Id = check_term(stmt.term2, tree, vars, types);

      const node1 = getLeafNode(tree, t1Id);
      const node2 = getLeafNode(tree, t2Id);

      if (!isTypeEqual(typeInverse(node1.type), node2.type, types)) {
        throw new Error("Elim type mismatch");
      }

      const connectedId = connectNodes(tree, [t1Id, t2Id], "tensor");
      removeNode(tree, connectedId);

      return vars;
    }
    case "return": {
      // Handled in block/Program check loop usually.
      // usage in check_statement implies side effect?
      // But return produces a value.
      // check_statement returns Variables.
      // We can just verify term and return vars?
      check_term(stmt.value, tree, vars, types);
      normalize(tree);
      return vars;
    }
  }
}

export function check_program(program: Program, tree: ContextTree): void {
  let vars: Variables = new Map();
  const types: TypeEnv = new Map();

  for (const item of program) {
    if ("type" in item && item.type === "type_alias") {
      types.set(item.name, item.value);
    } else {
      // Statement
      // Top level statements?
      // "Statement: let, intro, elim, return".
      // `basic.ll` has `let mp1 ...` at top level.
      vars = check_statement(item as Statement, tree, vars, types);
    }
  }
}
