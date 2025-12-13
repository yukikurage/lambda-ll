import { LLType } from "./lltype";

export type Term =
  | { type: "var"; name: string }
  | { type: "tensor"; elements: Term[] } // [t, ...]
  | { type: "par"; elements: Term[] } // {t, ...}
  | { type: "block"; statements: Statement[] }; // { s; ... }

export type TypeAlias = { type: "type_alias"; name: string; value: LLType };

export type Statement =
  | { type: "let"; name: string; typeAnnotation?: LLType; value: Term }
  | { type: "let_destruct_tensor"; names: string[]; value: Term }
  | { type: "let_destruct_par"; names: string[]; value: Term }
  | { type: "intro"; name1: string; name2: string; typeAnnotation: LLType }
  | { type: "elim"; term1: Term; term2: Term }
  | { type: "return"; value: Term };

export type ProgramItem = Statement | TypeAlias;
export type Program = ProgramItem[];
