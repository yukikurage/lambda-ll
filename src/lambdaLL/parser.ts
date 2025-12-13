import { Program, Statement, Term, TypeAlias, ProgramItem } from "./ast";
import {
  LLType,
  parType,
  primitiveType,
  tensorType,
  typeInverse,
} from "./lltype";

export type TokenType =
  | "let"
  | "intro"
  | "elim"
  | "return"
  | "type" // type alias
  | "Keyword" // let, intro, elim, return, type
  | "Symbol" // = : ; , * => ( ) [ ] { }
  | "Identifier"
  | "Number" // For numeric literals, if any
  | "EOF";

export type Token = {
  type: TokenType;
  value: string;
  pos: number;
  line: number;
  col: number;
};

export class Lexer {
  private input: string;
  private pos: number = 0;
  private line: number = 1;
  private lineStartPos: number = 0;

  constructor(input: string) {
    this.input = input;
  }

  private peek(n: number = 0): string {
    if (this.pos + n >= this.input.length) return "";
    return this.input[this.pos + n];
  }

  private advance(): string {
    const char = this.input[this.pos++];
    if (char === "\n") {
      this.line++;
      this.lineStartPos = this.pos;
    }
    return char;
  }

  tokenize(): Token[] {
    const tokens: Token[] = [];
    while (this.pos < this.input.length) {
      const char = this.peek();
      const currentLine = this.line;
      const currentCol = this.pos - this.lineStartPos + 1;
      const currentPos = this.pos; // Store current pos before advancing

      if (/\s/.test(char)) {
        this.advance();
        continue;
      }

      if (char === "#") {
        // Comment
        if (this.peek(1) === "-") {
          // Block Comment
          this.advance(); // #
          this.advance(); // -
          while (
            !(this.peek() === "-" && this.peek(1) === "#") &&
            this.pos < this.input.length
          ) {
            this.advance();
          }
          if (this.pos < this.input.length) {
            this.advance(); // -
            this.advance(); // #
          }
        } else {
          // Line comment
          while (this.peek() !== "\n" && this.pos < this.input.length) {
            this.advance();
          }
        }
        continue;
      }

      if (/[a-zA-Z_]/.test(char)) {
        const start = this.pos;
        while (/[a-zA-Z0-9_]/.test(this.peek())) {
          this.advance();
        }
        const value = this.input.slice(start, this.pos);
        let type: TokenType = "Identifier";
        if (["let", "intro", "elim", "return", "type"].includes(value)) {
          type = "Keyword";
        }
        tokens.push({
          type,
          value,
          pos: start,
          line: currentLine,
          col: currentCol,
        });
        continue;
      }

      if (/[0-9]/.test(char)) {
        // Number (not used but good to have)
        const start = this.pos;
        while (/[0-9]/.test(this.peek())) {
          this.advance();
        }
        const value = this.input.slice(start, this.pos);
        tokens.push({
          type: "Number",
          value,
          pos: start,
          line: currentLine,
          col: currentCol,
        });
        continue;
      }

      // Symbols
      const twoCharSymbols = ["=>"];
      const oneCharSymbols = [
        "=",
        ":",
        ";",
        ",",
        "*",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
      ];
      let matched = false;

      const twoChars = char + this.peek(1);
      if (twoCharSymbols.includes(twoChars)) {
        tokens.push({
          type: "Symbol",
          value: twoChars,
          pos: currentPos,
          line: currentLine,
          col: currentCol,
        });
        this.advance();
        this.advance();
        matched = true;
      } else if (oneCharSymbols.includes(char)) {
        tokens.push({
          type: "Symbol",
          value: char,
          pos: currentPos,
          line: currentLine,
          col: currentCol,
        });
        this.advance();
        matched = true;
      }

      if (!matched) {
        throw new Error(
          `Unexpected character: ${char} at line ${currentLine}, col ${currentCol}`
        );
      }
    }
    tokens.push({
      type: "EOF",
      value: "",
      pos: this.pos,
      line: this.line,
      col: this.pos - this.lineStartPos + 1,
    });
    return tokens;
  }
}

import { CompilerError } from "./compilerError";

export class Parser {
  private tokens: Token[];
  private current: number = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private advance(): Token {
    if (!this.isAtEnd()) {
      this.current++;
    }
    return this.previous();
  }

  private isAtEnd(): boolean {
    return this.peek().type === "EOF";
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private check(type: TokenType, value?: string): boolean {
    if (this.isAtEnd()) return false;
    if (this.peek().type !== type) return false;
    if (value !== undefined && this.peek().value !== value) return false;
    return true;
  }

  private match(type: TokenType, value?: string): boolean {
    if (this.check(type, value)) {
      this.advance();
      return true;
    }
    return false;
  }

  private consume(type: TokenType, message: string, value?: string): Token {
    if (this.check(type, value)) return this.advance();
    throw this.error(this.peek(), message);
  }

  private error(token: Token, message: string): CompilerError {
    // But since I already defined CompilerError with line/col,
    // I should probably make Lexer cleaner.

    // Let's make it throw a temporary structure or just let CodeEditor handle it.
    // But I want "proper" error handling.

    // I'll throw a temporary Error with property `pos` and catch it in editor.
    // But wait, I imported `CompilerError`.
    // I should update `Lexer` in next step to add Line info to tokens.
    // For now, I will throw generic Error with message and assume CodeEditor parses it?
    // No, that was the old way.

    // Let's modify `Lexer` to track lines.

    return new CompilerError(message, 0, 0, 1); // Placeholders
  }

  parseProgram(): Program {
    const items: ProgramItem[] = [];
    while (!this.isAtEnd()) {
      if (this.check("Keyword", "type")) {
        items.push(this.parseTypeAlias());
      } else if (this.check("Keyword", "let")) {
        items.push(this.parseStatement()); // parseStatement handles 'let'
      } else {
        throw this.error(
          this.peek(),
          "Expected top-level 'type' or 'let' declaration"
        );
      }
    }
    return items;
  }

  parseTypeAlias(): TypeAlias {
    this.consume("Keyword", "Expected 'type'", "type");
    const name = this.consume("Identifier", "Expected type name").value;
    this.consume("Symbol", "Expected '='", "=");
    const value = this.parseLLType();
    this.consume("Symbol", "Expected ';'", ";");
    return { type: "type_alias", name, value };
  }

  parseStatement(): Statement {
    if (this.match("Keyword", "let")) {
      // let ...
      if (this.match("Symbol", "{")) {
        // let {x1, ...} = t
        const names: string[] = [];
        do {
          names.push(
            this.consume("Identifier", "Expected variable name").value
          );
        } while (this.match("Symbol", ","));
        this.consume("Symbol", "Expected '}'", "}");
        this.consume("Symbol", "Expected '='", "=");
        const value = this.parseTerm();
        this.consume("Symbol", "Expected ';'", ";");
        return { type: "let_destruct_par", names, value };
      } else if (this.match("Symbol", "[")) {
        // let [x1, ...] = t
        const names: string[] = [];
        do {
          names.push(
            this.consume("Identifier", "Expected variable name").value
          );
        } while (this.match("Symbol", ","));
        this.consume("Symbol", "Expected ']'", "]");
        this.consume("Symbol", "Expected '='", "=");
        const value = this.parseTerm();
        this.consume("Symbol", "Expected ';'", ";");
        return { type: "let_destruct_tensor", names, value };
      } else {
        // let x [: T] = t
        const name = this.consume("Identifier", "Expected variable name").value;
        let typeAnnotation: LLType | undefined;
        if (this.match("Symbol", ":")) {
          typeAnnotation = this.parseLLType();
        }
        this.consume("Symbol", "Expected '='", "=");
        const value = this.parseTerm();
        this.consume("Symbol", "Expected ';'", ";");
        return { type: "let", name, typeAnnotation, value };
      }
    } else if (this.match("Keyword", "intro")) {
      // intro x, y : T
      const name1 = this.consume("Identifier", "Expected name").value;
      this.consume("Symbol", "Expected ','", ",");
      const name2 = this.consume("Identifier", "Expected name").value;
      this.consume("Symbol", "Expected ':'", ":");
      const typeAnnotation = this.parseLLType();
      this.consume("Symbol", "Expected ';'", ";");
      return { type: "intro", name1, name2, typeAnnotation };
    } else if (this.match("Keyword", "elim")) {
      // elim t1, t2
      const term1 = this.parseTerm();
      this.consume("Symbol", "Expected ','", ",");
      const term2 = this.parseTerm();
      this.consume("Symbol", "Expected ';'", ";");
      return { type: "elim", term1, term2 };
    } else if (this.match("Keyword", "return")) {
      const value = this.parseTerm();
      this.consume("Symbol", "Expected ';'", ";");
      return { type: "return", value };
    }
    throw this.error(
      this.peek(),
      `Unexpected start of statement: ${this.peek().value}`
    );
  }

  parseTerm(): Term {
    let term: Term;
    if (this.match("Identifier")) {
      term = { type: "var", name: this.previous().value };
    } else if (this.match("Symbol", "[")) {
      const elements: Term[] = [];
      do {
        elements.push(this.parseTerm());
      } while (this.match("Symbol", ","));
      this.consume("Symbol", "Expected ']'", "]");
      term = { type: "tensor", elements };
    } else if (this.match("Symbol", "{")) {
      // Can be Par {x, y} OR Block { stmt; ... }

      // Lookahead to disambiguate Par vs Block
      // Block starts with statement keywords: let, intro, elim, return
      const token = this.peek();
      const isBlock =
        token.type === "Keyword" &&
        ["let", "intro", "elim", "return"].includes(token.value);

      if (isBlock) {
        const statements: Statement[] = [];
        while (!this.check("Symbol", "}")) {
          statements.push(this.parseStatement());
        }
        this.consume("Symbol", "Expected '}'", "}");
        term = { type: "block", statements };
      } else {
        // Par
        const elements: Term[] = [];
        do {
          elements.push(this.parseTerm());
        } while (this.match("Symbol", ","));
        this.consume("Symbol", "Expected '}'", "}");
        term = { type: "par", elements };
      }
    } else if (this.match("Symbol", "(")) {
      // (t) or (t1, t2) or (x:A, y) => t

      const elements: { term: Term; type?: LLType }[] = [];

      // Empty () possibly?
      if (!this.check("Symbol", ")")) {
        do {
          const term = this.parseTerm();
          let type: LLType | undefined;
          if (this.match("Symbol", ":")) {
            type = this.parseLLType();
          }
          elements.push({ term, type });
        } while (this.match("Symbol", ","));
      }
      this.consume("Symbol", "Expected ')'", ")");

      if (this.match("Symbol", "=>")) {
        // It's a lambda!
        // Elements MUST be variables.
        const args: { name: string; type?: LLType }[] = [];
        for (const e of elements) {
          if (e.term.type !== "var") {
            throw this.error(
              this.previous(),
              "Lambda arguments must be variables"
            );
          }
          args.push({ name: e.term.name, type: e.type });
        }
        const body = this.parseTerm();
        term = { type: "lambda", args, body };
      } else {
        // Just (T) or (T1, T2)
        // Ensure no types were provided
        for (const e of elements) {
          if (e.type) {
            throw this.error(
              this.previous(),
              "Unexpected type annotation in tuple/expression"
            );
          }
        }

        if (elements.length === 1) term = elements[0].term;
        else term = { type: "tensor", elements: elements.map((e) => e.term) };
      }
    } else {
      throw this.error(
        this.peek(),
        `Unexpected start of term: ${this.peek().value}`
      );
    }

    // Check for App: term(args)
    while (this.match("Symbol", "(")) {
      const args: Term[] = [];
      if (!this.check("Symbol", ")")) {
        do {
          args.push(this.parseTerm());
        } while (this.match("Symbol", ","));
      }
      this.consume("Symbol", "Expected ')'", ")");
      term = { type: "app", func: term, args };
    }

    return term;
  }

  // Parsing Types: P, Q, T*, [T...], {T...}, (T...)=>T
  parseLLType(): LLType {
    return this.parseLLTypeAtom();
  }

  parseLLTypeAtom(): LLType {
    let type: LLType;
    if (this.match("Identifier")) {
      const name = this.previous().value;
      type = primitiveType(name, 1);
    } else if (this.match("Symbol", "[")) {
      const elements: LLType[] = [];
      do {
        elements.push(this.parseLLType());
      } while (this.match("Symbol", ","));
      this.consume("Symbol", "Expected ']'", "]");
      type = tensorType(elements);
    } else if (this.match("Symbol", "{")) {
      const elements: LLType[] = [];
      do {
        elements.push(this.parseLLType());
      } while (this.match("Symbol", ","));
      this.consume("Symbol", "Expected '}'", "}");
      type = parType(elements);
    } else if (this.match("Symbol", "(")) {
      const elements: LLType[] = [];
      if (!this.check("Symbol", ")")) {
        do {
          elements.push(this.parseLLType());
        } while (this.match("Symbol", ","));
      }
      this.consume("Symbol", "Expected ')'", ")");

      // Check if followed by =>
      if (this.match("Symbol", "=>")) {
        const right = this.parseLLType();
        const lefts = elements.map((t) => typeInverse(t));
        // (A, B) => C is {A*, B*, C}
        // Mark preference as "function"
        return parType([...lefts, right], "function");
      }

      // Just (T) or (T1, T2)
      if (elements.length === 1) type = elements[0];
      else type = tensorType(elements);
    } else {
      throw this.error(
        this.peek(),
        `Unexpected start of type: ${this.peek().value}`
      );
    }

    // Handle postfix operators like * or => (if not parenthesized)
    // Wait, `A => B` without parens?
    // Current parser structure handles atoms. type* is handled here?
    // Original code loop handled *. Let's reinstate that loop structure if needed.
    // Actually, type* connects to the specific atom.
    while (this.match("Symbol", "*")) {
      type = typeInverse(type);
    }

    // Handle A => B (infix) ??
    // The previous implementation had `parseLLType` calling `parseLLTypeAtom` then checking `=>`.
    // Let's support `A => B` at the top level.
    if (this.match("Symbol", "=>")) {
      const right = this.parseLLType();
      type = parType([typeInverse(type), right], "function");
    }

    return type;
  }
}
