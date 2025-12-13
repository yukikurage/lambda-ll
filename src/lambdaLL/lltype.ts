export type LLType =
  | { type: "primitive"; name: string; spin: 1 | -1 } // A, B, C, ...
  | { type: "tensor"; elements: LLType[] } // [t, ...]
  | { type: "par"; elements: LLType[] } // {t, ...}
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

export function tensorType(elements: LLType[]): LLType {
  return { type: "tensor", elements };
}

export function parType(elements: LLType[]): LLType {
  return { type: "par", elements };
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
      return parType(type.elements.map(typeInverse));
    case "par":
      return tensorType(type.elements.map(typeInverse));
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
