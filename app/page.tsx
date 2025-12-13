"use client";

import { useState, useCallback } from "react";
import { CodeEditor } from "../src/components/CodeEditor";
import { ContextTree } from "../src/lambdaLL/contextTree";
import { Variables } from "../src/lambdaLL/typeChecker";
import { formatType } from "../src/lambdaLL/lltype";

const initialCode = `#-
Types: P, Q,..., P*, Q*,... [P,...], {P,...},

Inverse: 
P* = P*
[P, Q,...]* = {P*, Q*,...}
{P, Q,...}* = [P*, Q*,...]
-#

# (A, B) => C is syntax suger for {A*, B*, C}
# (P => Q, P) => Q 
#   = {(P => Q)*, P*, Q}
#   = {{P*, Q}*, P*, Q}
#   = {[P, Q*], P*, Q} 
type MP = (P => Q, P) => Q;

let mp1 : MP = {
  # introduce variables of types A* and A
  # arg1: (P => Q)* = {P*, Q}* = [P, Q*]
  intro arg1, pToQ : P => Q; 
  # arg2: P*, p : P
  intro arg2, p : P;
  # destruct pToQ : {P*, Q} into pInv : P* and q : Q 
  let {pInv, q} = pToQ;
  # eliminate terms of types A* and A
  # here, p : P and pInv : P*
  elim p, pInv;
  # return {arg1, arg2, q} : {[P, Q*], P*, Q} = MP
  return {arg1, arg2, q};
};

# with syntax suger
# (x, y) => t is syntax suger for {intro _x, x; intro _y, y; return {_x, _y, t}}
# f(t, u) is syntax suger for 
#   {
#     {_arg1, _arg2, _result} = f;
#     elim _arg1, t; 
#     elim _arg2, u;
#     return _result;
#   } 
let mp2 : MP = (f : P => Q, x : P) => f(x);

type Inner = [P, {Q, R}] => {Q, [P, R]};

let inner : Inner = (pqr : [P, {Q, R}]) => {
  let [p, qr] = pqr;
  let {q, r} = qr;
  return {q, [p, r]};
};

`;

export default function Home() {
  const [proofStatus, setProofStatus] = useState<
    | { type: "success"; vars: Variables; tree: ContextTree }
    | { type: "error"; message: string }
    | null
  >(null);

  const handleProofUpdate = useCallback(
    (
      res:
        | { type: "success"; vars: Variables; tree: ContextTree }
        | { type: "error"; message: string }
    ) => {
      if (res.type === "success") {
        setProofStatus({ type: "success", vars: res.vars, tree: res.tree });
      } else {
        setProofStatus({ type: "error", message: res.message });
      }
    },
    []
  );

  return (
    <div className="flex flex-col h-screen bg-neutral-900 text-white">
      <header className="p-4 border-b border-neutral-800 bg-neutral-950 flex items-center justify-between">
        <h1 className="text-xl font-bold bg-linear-to-r from-blue-400 to-purple-500 bg-clip-text text-transparent">
          Lambda-LL Type Checker
        </h1>
      </header>

      <main className="flex-1 flex overflow-hidden">
        {/* Editor Pane */}
        <div className="w-1/2 h-full p-4 border-r border-neutral-800">
          <CodeEditor
            initialCode={initialCode}
            onProofUpdate={handleProofUpdate}
          />
        </div>

        {/* Info / Visualizer Pane */}
        <div className="w-1/2 h-full p-6 overflow-auto bg-neutral-950/50">
          {proofStatus?.type === "error" && (
            <div className="p-4 rounded bg-red-900/20 border border-red-500/50 text-red-200">
              <h2 className="font-bold mb-2">Type Error</h2>
              <pre className="whitespace-pre-wrap font-mono text-sm">
                {proofStatus.message}
              </pre>
            </div>
          )}

          {proofStatus?.type === "success" && (
            <div className="space-y-6">
              <div className="p-4 rounded bg-green-900/20 border border-green-500/50 text-green-200">
                <h2 className="font-bold mb-2">Proof Verified ✅</h2>
                <div className="space-y-2">
                  {Array.from(proofStatus.vars.entries()).map(([name, id]) => {
                    // Lookup type from tree
                    const node = proofStatus.tree.structure.get(id);
                    let typeStr = "...";
                    if (node && node.nodeType === "leaf") {
                      typeStr = formatType(node.type);
                    }

                    return (
                      <div
                        key={`${name}-${id}`}
                        className="flex items-center gap-2 font-mono text-sm"
                      >
                        <span className="text-purple-400">let</span>
                        <span className="text-yellow-200">{name}</span>
                        <span className="text-neutral-500">:</span>
                        <span className="text-blue-300">{typeStr}</span>
                      </div>
                    );
                  })}
                </div>
              </div>
            </div>
          )}

          {!proofStatus && (
            <div className="text-neutral-500 italic">
              Type some code to see results...
            </div>
          )}
        </div>
      </main>
    </div>
  );
}
