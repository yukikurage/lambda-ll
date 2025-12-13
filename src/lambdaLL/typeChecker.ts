import { Statement, Term, Program } from "./ast";
import {
  ContextId,
  ContextTree,
  addBranchNode,
  addLeafNode,
  addRootNode,
  connectNodes,
  destructNode,
  getLeafNode,
  getNode,
  normalize,
  removeNode,
} from "./contextTree";
import { LLType, typeInverse, tensorType, parType, formatType } from "./lltype";

export type Variables = Map<string, ContextId>; // variable name -> leaf node id
export type TypeEnv = Map<string, LLType>; // type alias -> LLType

function resolveType(t: LLType, env: TypeEnv): LLType {
  if (t.type === "primitive") {
    if (env.has(t.name)) {
      // Check spin?
      // If alias is T = [A]. T* = [A]*.
      // Logic: standard LL alias is usually for the positive type.
      // If t is T*. `resolve(T)` is `[A]`. `T*` is `[A]*`.
      // How to strict parsing?
      // `primitiveType` has `spin`.
      // If we aliased `MP = ...`. `MP` is primitive "MP".
      // If usage `MP`. `spin` is 1. result is `resolved`.
      // If usage `MP*`. `spin` is -1. result is `resolved*`.
      // We need `typeInverse` to handle complex types properly if `resolved` covers it.
      // Yes `typeInverse` does that.
      const resolved = env.get(t.name)!;
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

function isDescendant(
  tree: ContextTree,
  childId: ContextId,
  ancestorId: ContextId
): boolean {
  if (childId === ancestorId) return true;
  let curr = tree.structure.get(childId);
  while (curr && curr.parent !== undefined) {
    if (curr.parent === ancestorId) return true;
    curr = tree.structure.get(curr.parent);
  }
  return false;
}

function propagatePreference(target: LLType, source: LLType) {
  if (target.type !== source.type) return;

  if (target.type === "par" && source.type === "par") {
    if (source.preference) target.preference = source.preference;
    if (target.elements.length === source.elements.length) {
      target.elements.forEach((e, i) =>
        propagatePreference(e, source.elements[i])
      );
    }
  }
  if (target.type === "tensor" && source.type === "tensor") {
    if (source.preference) target.preference = source.preference;
    if (target.elements.length === source.elements.length) {
      target.elements.forEach((e, i) =>
        propagatePreference(e, source.elements[i])
      );
    }
  }
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
      let currentVars = deepCopyVariables(vars); // Scoped vars

      // Execute statements
      for (const stmt of term.statements) {
        if (stmt.type === "return") {
          const retId = check_term(stmt.value, tree, currentVars, types);
          normalize(tree);

          // Linearity Check: Ensure all local variables are consumed (must be part of retId tree)
          for (const [name, id] of currentVars.entries()) {
            if (!vars.has(name)) {
              // It is a local variable
              if (tree.structure.has(id)) {
                // If it exists in tree
                if (!isDescendant(tree, id, retId)) {
                  throw new Error(
                    `Linear variable '${name}' is not consumed (not part of return value).`
                  );
                }
              }
              // If not in tree (consumed by elim/destruct?), that's fine?
              // Wait, elim removes the tensor node. The variables were connected to it.
              // connectNodes(tensor) removed original leaves?
              // Tensor logic `connectNode` uses `mergeBranch`. It does NOT remove original leaves.
              // Par logic `connectNode` uses `removeNode`.
              // So for Tensor vars, they SHOULD be in tree.
              // If they are missing, it's weird?
              // But `if (tree.structure.has(id))` safety check is fine.
            }
          }

          return retId;
        } else {
          currentVars = check_statement(stmt, tree, currentVars, types);
        }
      }
      throw new Error("Block must end with return");
    }
    case "lambda": {
      // (args) => body
      // Desugar to: block { intro _arg1, arg1 : Type; ... return {_arg1, ..., body} }
      const statements: Statement[] = [];
      const returnElements: Term[] = [];

      for (const arg of term.args) {
        if (!arg.type) {
          throw new Error("Lambda argument types are required");
        }
        const internalName = `_${arg.name}_neg`;
        // intro _x, x : T
        statements.push({
          type: "intro",
          name1: internalName,
          name2: arg.name,
          typeAnnotation: arg.type,
        });
        returnElements.push({ type: "var", name: internalName });
      }

      returnElements.push(term.body);

      const blockTerm: Term = {
        type: "block",
        statements: [
          ...statements,
          {
            type: "return",
            value: { type: "par", elements: returnElements },
          },
        ],
      };

      const resultId = check_term(blockTerm, tree, vars, types);

      // Tag with preference
      const node = getLeafNode(tree, resultId);
      if (node.type.type === "par") {
        node.type.preference = "function";
      }

      return resultId;
    }
    case "app": {
      // func(args)
      // Desugar to:
      // block {
      //   let {_arg1, ..., _ret} = func;
      //   elim _arg1, arg1;
      //   ...
      //   return _ret;
      // }

      const statements: Statement[] = [];
      const retName = "_ret";
      const argNames: string[] = [];

      term.args.forEach((_, i) => argNames.push(`_arg_${i}`));

      // let { ... } = func
      statements.push({
        type: "let_destruct_par",
        names: [...argNames, retName],
        value: term.func,
      });

      // elim
      term.args.forEach((arg, i) => {
        statements.push({
          type: "elim",
          term1: { type: "var", name: argNames[i] },
          term2: arg,
        });
      });

      // return
      statements.push({
        type: "return",
        value: { type: "var", name: retName },
      });

      const blockTerm: Term = { type: "block", statements };
      return check_term(blockTerm, tree, vars, types);
    }
  }
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
          throw new Error(
            `Type mismatch for let '${stmt.name}': expected ${formatType(
              stmt.typeAnnotation
            )}, got ${formatType(tNode.type)}`
          );
        }

        // Propagate preferences recursively
        const resolved = resolveType(stmt.typeAnnotation, types);
        propagatePreference(tNode.type, resolved);
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
      const root = getNode(tree, tree.root);
      // Intro requires Root to be a Tensor Branch.
      // If it is a Leaf (e.g. previous result) or Par, wrap it in a Tensor.
      if (
        root.nodeType === "leaf" ||
        (root.nodeType === "branch" && root.branchType !== "tensor")
      ) {
        addRootNode(tree, "tensor");
        // tree.root is now updated
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

export function check_program(program: Program, tree: ContextTree): Variables {
  let vars: Variables = new Map();
  const types: TypeEnv = new Map();

  for (const item of program) {
    if ("type" in item && item.type === "type_alias") {
      types.set(item.name, item.value);
    } else {
      // Statement
      vars = check_statement(item as Statement, tree, vars, types);
    }
  }
  return vars;
}
