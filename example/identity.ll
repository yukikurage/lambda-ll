#-
Identity Function: P => P
Type: {~P, P}
-#

type ID = P => P;

let id : ID = (x : P) => x;

# Manual expansion
let id_manual : ID = {
  intro input, output : P;
  # input: P, output: ~P ? No.
  # intro creates connected P, P.
  # We want to form {~P, P}.
  # Let's see: `linkWeak target [input, output]`.
  # This matches `(x:P) => x`.
  # elim input, output; # Removed: elim consumes them. We just want to return the connected pair. 
  # Wait, manual construction of Identity in LL:
  # Axiom P connects ~P and P.
  # Here we need a term of type {~P, P}.
  # `intro` creates an Axiom link between two vars.
  # We just return them?
  return {input, output};
};
