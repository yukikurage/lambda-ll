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
  | "operator" // = : ; , * =>
  | "paren_open" // (
  | "paren_close" // )
  | "bracket_open" // [
  | "bracket_close" // ]
  | "brace_open" // {
  | "brace_close" // }
  | "identifier"
  | "eof";

export type Token = {
  type: TokenType;
  value: string;
  pos: number;
};

export class Lexer {
  input: string;
  pos: number;

  constructor(input: string) {
    this.input = input;
    this.pos = 0;
  }

  tokenize(): Token[] {
    const tokens: Token[] = [];
    while (this.pos < this.input.length) {
      const char = this.input[this.pos];

      if (/\s/.test(char)) {
        this.pos++;
        continue;
      }

      if (this.input.startsWith("#-", this.pos)) {
        // Block comment
        this.pos += 2;
        while (
          this.pos < this.input.length &&
          !this.input.startsWith("-#", this.pos)
        ) {
          this.pos++;
        }
        if (this.input.startsWith("-#", this.pos)) {
          this.pos += 2;
        }
        continue;
      }

      if (char === "#") {
        // Line Comment
        while (this.pos < this.input.length && this.input[this.pos] !== "\n") {
          this.pos++;
        }
        continue;
      }

      if (/[a-zA-Z]/.test(char)) {
        let value = "";
        while (
          this.pos < this.input.length &&
          /[a-zA-Z0-9]/.test(this.input[this.pos])
        ) {
          value += this.input[this.pos];
          this.pos++;
        }
        switch (value) {
          case "let":
          case "intro":
          case "elim":
          case "return":
          case "type":
            tokens.push({
              type: value as TokenType,
              value,
              pos: this.pos - value.length,
            });
            break;
          default:
            tokens.push({
              type: "identifier",
              value,
              pos: this.pos - value.length,
            });
            break;
        }
        continue;
      }

      const operatorMatch = this.input.slice(this.pos).match(/^(=>|[:=;,*])/);
      if (operatorMatch) {
        tokens.push({
          type: "operator",
          value: operatorMatch[0],
          pos: this.pos,
        });
        this.pos += operatorMatch[0].length;
        continue;
      }

      if ("()[]{}".includes(char)) {
        const typeMap: { [key: string]: TokenType } = {
          "(": "paren_open",
          ")": "paren_close",
          "[": "bracket_open",
          "]": "bracket_close",
          "{": "brace_open",
          "}": "brace_close",
        };
        tokens.push({ type: typeMap[char], value: char, pos: this.pos });
        this.pos++;
        continue;
      }

      throw new Error(`Unexpected character: ${char} at ${this.pos}`);
    }
    tokens.push({ type: "eof", value: "", pos: this.pos });
    return tokens;
  }
}

export class Parser {
  tokens: Token[];
  pos: number;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.pos = 0;
  }

  peek(): Token {
    return this.tokens[this.pos];
  }

  consume(type: TokenType, value?: string): Token {
    const token = this.peek();
    if (token.type !== type || (value && token.value !== value)) {
      throw new Error(
        `Expected ${type} ${value || ""}, got ${token.type} ${token.value} at ${
          token.pos
        }`
      );
    }
    this.pos++;
    return token;
  }

  match(type: TokenType, value?: string): boolean {
    const token = this.peek();
    return token.type === type && (!value || token.value === value);
  }

  parseProgram(): Program {
    const items: ProgramItem[] = [];
    while (!this.match("eof")) {
      if (this.match("type")) {
        items.push(this.parseTypeAlias());
      } else {
        items.push(this.parseStatement());
      }
    }
    return items;
  }

  parseTypeAlias(): TypeAlias {
    this.consume("type");
    const name = this.consume("identifier").value;
    this.consume("operator", "=");
    const value = this.parseLLType();
    this.consume("operator", ";");
    return { type: "type_alias", name, value };
  }

  parseStatement(): Statement {
    if (this.match("let")) {
      this.consume("let");
      if (this.match("brace_open")) {
        // let {x1, ...} = t
        this.consume("brace_open");
        const names: string[] = [];
        while (!this.match("brace_close")) {
          names.push(this.consume("identifier").value);
          if (this.match("operator", ",")) this.consume("operator", ",");
        }
        this.consume("brace_close");
        this.consume("operator", "=");
        const value = this.parseTerm();
        this.consume("operator", ";");
        return { type: "let_destruct_par", names, value };
      } else if (this.match("bracket_open")) {
        // let [x1, ...] = t
        this.consume("bracket_open");
        const names: string[] = [];
        while (!this.match("bracket_close")) {
          names.push(this.consume("identifier").value);
          if (this.match("operator", ",")) this.consume("operator", ",");
        }
        this.consume("bracket_close");
        this.consume("operator", "=");
        const value = this.parseTerm();
        this.consume("operator", ";");
        return { type: "let_destruct_tensor", names, value };
      } else {
        // let x [: T] = t
        const name = this.consume("identifier").value;
        let typeAnnotation: LLType | undefined;
        if (this.match("operator", ":")) {
          this.consume("operator", ":");
          typeAnnotation = this.parseLLType();
        }
        this.consume("operator", "=");
        const value = this.parseTerm();
        this.consume("operator", ";");
        return { type: "let", name, typeAnnotation, value };
      }
    } else if (this.match("intro")) {
      // intro x, y : T
      this.consume("intro");
      const name1 = this.consume("identifier").value;
      this.consume("operator", ",");
      const name2 = this.consume("identifier").value;
      this.consume("operator", ":");
      const typeAnnotation = this.parseLLType();
      this.consume("operator", ";");
      return { type: "intro", name1, name2, typeAnnotation };
    } else if (this.match("elim")) {
      // elim t1, t2
      this.consume("elim");
      const term1 = this.parseTerm();
      this.consume("operator", ",");
      const term2 = this.parseTerm();
      this.consume("operator", ";");
      return { type: "elim", term1, term2 };
    } else if (this.match("return")) {
      this.consume("return");
      const value = this.parseTerm();
      this.consume("operator", ";");
      return { type: "return", value };
    }
    throw new Error(
      `Unexpected start of statement: ${this.peek().value} at ${
        this.peek().pos
      }`
    );
  }

  parseTerm(): Term {
    if (this.match("identifier")) {
      return { type: "var", name: this.consume("identifier").value };
    } else if (this.match("bracket_open")) {
      this.consume("bracket_open");
      const elements: Term[] = [];
      while (!this.match("bracket_close")) {
        elements.push(this.parseTerm());
        if (this.match("operator", ",")) this.consume("operator", ",");
      }
      this.consume("bracket_close");
      return { type: "tensor", elements };
    } else if (this.match("brace_open")) {
      // Can be Par {x, y} OR Block { stmt; ... }
      this.consume("brace_open");

      // Check lookup
      const token = this.peek();
      const isBlock = ["let", "intro", "elim", "return"].includes(token.type);

      if (isBlock) {
        const statements: Statement[] = [];
        while (!this.match("brace_close")) {
          statements.push(this.parseStatement());
          // Statements end with ; inside parseStatement usually?
          // My parseStatement consumes ;.
        }
        this.consume("brace_close");
        return { type: "block", statements };
      } else {
        // Par
        const elements: Term[] = [];
        while (!this.match("brace_close")) {
          elements.push(this.parseTerm());
          if (this.match("operator", ",")) this.consume("operator", ",");
        }
        this.consume("brace_close");
        return { type: "par", elements };
      }
    } else if (this.match("paren_open")) {
      this.consume("paren_open");
      // (t) or (t1, t2)
      const elements: Term[] = [];
      while (!this.match("paren_close")) {
        elements.push(this.parseTerm());
        if (this.match("operator", ",")) this.consume("operator", ",");
      }
      this.consume("paren_close");
      if (elements.length === 1) return elements[0];
      // (Term tuple) -> treat as Tensor if user allows?
      // User example: return {(p, arg1), arg2} -> (p, arg1) is tensor.
      return { type: "tensor", elements };
    }
    throw new Error(
      `Unexpected start of term: ${this.peek().value} at ${this.peek().pos}`
    );
  }

  // Parsing Types: P, Q, T*, [T...], {T...}, (T...)=>T
  parseLLType(): LLType {
    let type = this.parseLLTypeAtom();

    while (true) {
      if (this.match("operator", "*")) {
        this.consume("operator", "*");
        type = typeInverse(type);
      } else if (this.match("operator", "=>")) {
        this.consume("operator", "=>");
        const right = this.parseLLType(); // right associative?
        type = parType([typeInverse(type), right]);
      } else {
        break;
      }
    }
    return type;
  }

  parseLLTypeAtom(): LLType {
    if (this.match("identifier")) {
      const name = this.consume("identifier").value;
      // Assuming primitive types for now (or alias name)
      return primitiveType(name, 1);
    } else if (this.match("bracket_open")) {
      this.consume("bracket_open");
      const elements: LLType[] = [];
      while (!this.match("bracket_close")) {
        elements.push(this.parseLLType());
        if (this.match("operator", ",")) this.consume("operator", ",");
      }
      this.consume("bracket_close");
      return tensorType(elements);
    } else if (this.match("brace_open")) {
      this.consume("brace_open");
      const elements: LLType[] = [];
      while (!this.match("brace_close")) {
        elements.push(this.parseLLType());
        if (this.match("operator", ",")) this.consume("operator", ",");
      }
      this.consume("brace_close");
      return parType(elements);
    } else if (this.match("paren_open")) {
      this.consume("paren_open");
      const elements: LLType[] = [];
      while (!this.match("paren_close")) {
        elements.push(this.parseLLType());
        if (this.match("operator", ",")) this.consume("operator", ",");
      }
      this.consume("paren_close");

      // Check if followed by =>
      if (this.match("operator", "=>")) {
        this.consume("operator", "=>");
        const right = this.parseLLType();
        const lefts = elements.map((t) => typeInverse(t));
        // (A, B) => C is {A*, B*, C}
        return parType([...lefts, right]);
      }

      // Just (T) or (T1, T2)
      if (elements.length === 1) return elements[0];
      return tensorType(elements);
    }
    throw new Error(
      `Unexpected start of type: ${this.peek().value} at ${this.peek().pos}`
    );
  }
}
