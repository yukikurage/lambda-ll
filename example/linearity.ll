#-
Linearity Failure
Unused variable.
-#

type Simple = P;

let unused : Simple = {
  intro x, y : P;
  # We use x but not y.
  return x;
};
