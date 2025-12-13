export type LLType =
  | { type: "primitive"; name: string; spin: 1 | -1 } // A, B, C, ...
  | { type: "tensor"; elements: LLType[]; preference?: "function" | null } // [t, ...]
  | { type: "par"; elements: LLType[]; preference?: "function" | null } // {t, ...}
  | { type: "with"; first: LLType; second: LLType } // (t & t)
  | { type: "plus"; left: LLType; right: LLType } // (t | t)
  | { type: "one" } // ()
  | { type: "bottom" } // {}
  | { type: "top" } // unknown
  | { type: "zero" }; // never

export type LLTypeString =
  | "primitive"
  | "tensor"
  | "par"
  | "with"
  | "plus"
  | "one"
  | "bottom"
  | "top"
  | "zero";

export function primitiveType(name: string, spin: 1 | -1): LLType {
  return { type: "primitive", name, spin };
}

export function tensorType(
  elements: LLType[],
  preference?: "function" | null
): LLType {
  return { type: "tensor", elements, preference };
}

export function parType(
  elements: LLType[],
  preference?: "function" | null
): LLType {
  return { type: "par", elements, preference };
}

export function withType(first: LLType, second: LLType): LLType {
  return { type: "with", first, second };
}

export function plusType(left: LLType, right: LLType): LLType {
  return { type: "plus", left, right };
}

export function oneType(): LLType {
  return { type: "one" };
}

export function bottomType(): LLType {
  return { type: "bottom" };
}

export function topType(): LLType {
  return { type: "top" };
}

export function zeroType(): LLType {
  return { type: "zero" };
}

export function typeInverse(type: LLType): LLType {
  switch (type.type) {
    case "primitive":
      return {
        type: "primitive",
        name: type.name,
        spin: type.spin === 1 ? -1 : 1,
      };
    case "tensor":
      return parType(type.elements.map(typeInverse), type.preference);
    case "par":
      return tensorType(type.elements.map(typeInverse), type.preference);
    case "with":
      return plusType(typeInverse(type.first), typeInverse(type.second));
    case "plus":
      return withType(typeInverse(type.left), typeInverse(type.right));
    case "one":
      return bottomType();
    case "bottom":
      return oneType();
    case "top":
      return zeroType();
    case "zero":
      return topType();
  }
}

// Helper to format type for display
export function formatType(t: LLType): string {
  if (t.type === "primitive") return t.spin === 1 ? t.name : `${t.name}*`;
  if (t.type === "tensor") return `[${t.elements.map(formatType).join(", ")}]`;
  if (t.type === "par") {
    if (t.preference === "function" && t.elements.length >= 2) {
      // Last element is return type
      // Inputs are negated.
      // (A, B) => C  means {A*, B*, C}
      // So we inverse inputs back to normal.
      const args = t.elements.slice(0, -1).map(typeInverse).map(formatType);
      const ret = formatType(t.elements[t.elements.length - 1]);
      if (args.length === 1) {
        return `${args[0]} => ${ret}`;
      } else {
        return `(${args.join(", ")}) => ${ret}`;
      }
    }
    return `{${t.elements.map(formatType).join(", ")}}`;
  }
  return "?";
}
