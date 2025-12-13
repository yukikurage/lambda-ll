"use client";

import { useState, useCallback } from "react";
import { CodeEditor } from "../src/components/CodeEditor";
import { ContextTree } from "../src/lambdaLL/contextTree";
import { Variables } from "../src/lambdaLL/typeChecker";
import { LLType, formatType } from "../src/lambdaLL/lltype";

const initialCode = `#-
  Type your linear logic proof here! 
  Refer to example/basic.ll for syntax.
-#

type MP = ((P) => Q, P) => Q;

let mp1 : MP = {
  intro arg1, pToQ : (P) => Q;
  intro arg2, p : P;
  let {pTo, q} = pToQ;
  elim p, pTo;
  return {arg1, arg2, q};
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
