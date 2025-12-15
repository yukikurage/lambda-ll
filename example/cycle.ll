#-
Cycle Test
Attempt to create a loop that violates Contractibility.
A simple way is to define a valid structure but somehow wire it backwards?
Or use Tensor Strong links to loop.
[x, y] with x, y connected.
-#

type Cycle = [P*, P];

let bad_cycle : Cycle = {
   intro x, y : P;
   # x, y are connected (Strong Axiom).
   # We return [x, y]. 
   # Tensor Intro [x, y] creates Strong link [x, y, result].
   # So x, y, result are all merged.
   # But x, y were ALREADY connected.
   # This should be a Cycle Error -> "Hard loop detected"
   return [x, y];
};
