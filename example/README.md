# Lambda-LL Language Guide

Lambda-LL is a linear logic programming language validated using the Danos-Regnier contractibility criterion.

## Syntax

### Types

- **Atoms**: `P`, `Q`, `Msg`, etc.
- **Dual**: `T*` (e.g. `P*`). Note: `(P*)* = P`.
- **Tensor**: `[A, B]` (Multiplicative Conjunction). Strong connection.
- **Par**: `{A, B}` (Multiplicative Disjunction). Weak connection.
- **Implication**: `A => B` (Sugar for `{A*, B}`).

### Terms

- **Variables**: `x`, `y`.
- **Constructors**:
  - `[t, u]`: Construct Tensor.
  - `{t, u}`: Construct Par.
- **Block**: `{ stmt; stmt; return t }`.
- **Function**: `(x:T) => t` (Sugar for Par Intro/Lambda).
- **Application**: `f(x)` (Sugar for Cut/Elim).

### Statements

- **Intro**: `intro x, y : T;`
  - Creates two variables `x` (:T) and `y` (:T\*) connected by an Axiom link.
- **Elim**: `elim x, y;`
  - Consumes `x` and `y` by connecting them with a Cut link.
  - `x` and `y` must have compatible types (duals).
- **Let**: `let x = t;`
  - Binds name `x` to term `t`.
- **Tensor Elim**: `let [x, y] = t;`
  - Destructs tensor `t` into `x` and `y`.
- **Par Elim**: `let {x, y} = t;`
  - Destructs par `t` into `x` and `y`.

## Example: Identity

```
type ID = P => P;
let id : ID = (x : P) => x;
```

## Validation

The compiler checks:

1.  **Linearity**: Every introduced variable must be used exactly once.
2.  **Types**: Terms must match declared types.
3.  **Topology (DR)**: The resulting Proof Net must be:
    - **Connected** (Single Component).
    - **Acyclic** (No Strong Cycles).
    - **Contractible** (Weak links satisfy Danos-Regnier criterion).
