import React, { useState, useEffect, useRef } from "react";
import Editor, { OnMount, Monaco } from "@monaco-editor/react";
import { editor } from "monaco-editor";
import { Lexer, Parser } from "../lambdaLL/parser";
import { check_program, Variables } from "../lambdaLL/typeChecker";
import { ContextTree, createContextTree } from "../lambdaLL/contextTree";
import { CompilerError } from "../lambdaLL/compilerError";

// Debounce helper
function useDebounce<T>(value: T, delay: number): T {
  const [debouncedValue, setDebouncedValue] = useState(value);
  useEffect(() => {
    const handler = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);
    return () => {
      clearTimeout(handler);
    };
  }, [value, delay]);
  return debouncedValue;
}

type EditorProps = {
  initialCode?: string;
  onProofUpdate: (
    result:
      | { type: "success"; vars: Variables; tree: ContextTree }
      | { type: "error"; message: string }
  ) => void;
};

export const CodeEditor: React.FC<EditorProps> = ({
  initialCode = "",
  onProofUpdate,
}) => {
  const [code, setCode] = useState(initialCode);
  const debouncedCode = useDebounce(code, 500);
  const editorRef = useRef<editor.IStandaloneCodeEditor | null>(null);
  const monacoRef = useRef<Monaco | null>(null);

  const [isEditorReady, setIsEditorReady] = useState(false);

  const handleEditorDidMount: OnMount = (editor, monaco) => {
    editorRef.current = editor;
    monacoRef.current = monaco;
    setIsEditorReady(true);
  };

  // Validation Effect
  useEffect(() => {
    if (!isEditorReady || !monacoRef.current || !editorRef.current) return;

    const model = editorRef.current.getModel();
    if (!model) return;

    try {
      const lexer = new Lexer(debouncedCode);
      const tokens = lexer.tokenize();
      const parser = new Parser(tokens);
      const program = parser.parseProgram();

      const tree = createContextTree("tensor");
      const vars = check_program(program, tree);

      // Success
      monacoRef.current.editor.setModelMarkers(model, "owner", []);
      onProofUpdate({ type: "success", vars, tree });
    } catch (e: unknown) {
      console.error(e);
      let message = "";
      if (e instanceof Error) {
        message = e.message;
      } else {
        message = String(e);
      }

      let startLine = 1;
      let startCol = 1;
      let endLine = 1;
      let endCol = 1;

      if (e instanceof CompilerError) {
        startLine = e.line;
        startCol = e.column;
        endLine = e.line;
        endCol = e.column + e.length;
      } else {
        // Fallback or other errors (e.g. Type Checking errors if they throw generic Error)
        const match = message.match(/at line (\d+), col (\d+)/);
        if (match) {
          startLine = parseInt(match[1]);
          startCol = parseInt(match[2]);
          endLine = startLine;
          endCol = startCol + 1;
        }
      }

      // Ensure lines are valid (Monaco lines are 1-based)
      if (startLine < 1) startLine = 1;
      if (endLine < startLine) endLine = startLine;

      monacoRef.current.editor.setModelMarkers(model, "owner", [
        {
          startLineNumber: startLine,
          startColumn: startCol,
          endLineNumber: endLine,
          endColumn: endCol,
          message: message,
          severity: monacoRef.current.MarkerSeverity.Error,
        },
      ]);
      onProofUpdate({ type: "error", message });
    }
  }, [debouncedCode, onProofUpdate, isEditorReady]);

  return (
    <div className="h-full w-full border rounded-md overflow-hidden">
      <Editor
        height="100%"
        defaultLanguage="python" // No specific LL syntax highlighting yet
        value={code}
        onChange={(value) => setCode(value || "")}
        onMount={handleEditorDidMount}
        theme="vs-dark"
        options={{
          minimap: { enabled: false },
          scrollBeyondLastLine: false,
          fontSize: 14,
        }}
      />
    </div>
  );
};
