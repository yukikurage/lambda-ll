#-
tensor : [x1, ..., xn]
par: {x1, ..., xn}
with: (x1 & x2) (between two types, not between n types)
plus: (x1 | x2)
one: unit    (unit of tesor)
bottom: request x by x;    (unit of par)
top: unknown  (unit of with)
zero: never   (unit of plus)
prim type: A, B, C,... P, Q, R, ...
type inverse: t*

(x1, ..., xn) => y is syntax suger for {x1*, ..., xn*, y}


(P)* = P*
[x1, ..., xn]* = {x1*, ..., xn*}
{x1, ..., xn}* = [x1*, ..., xn*]
(x1 | x2)* = (x1* & x2*)
(x1 & x2)* = (x1* | x2*)


ex. mp : (((P) => Q, P) => Q)
~ {(P => Q)*, P*, Q}
~ {{P*, Q}*, P*, Q}
~ {[P, Q*], P*, Q}


Statements:

intro x, x;
elim t, t;
let x = t;
-#


type MP = ((P) => Q, P) => Q;

# Valid proofs

let mp1 : MP = {
  intro arg1, pToQ : (P) => Q;   # intro arg1 : [P, Q*] and pToQ : {P*, Q}
  intro arg2, p : P;             # intro arg2 : P* and p : P
  let {pTo, q} = pToQ;           # let pTo : P* and q : Q from pToQ : {P*, Q}
  elim p, pTo;                   # elim p : P and pTo : P*
  return {arg1, arg2, q};        # return {arg1, arg2, q} : {[P, Q*], P*, Q}
};

let mp2 : MP = {
  intro arg1, pToQ : (P) => Q;   # intro arg1 : [P, Q*] and pToQ : {P*, Q}
  let {pTo, q} = pToQ;           # let pTo : P* and q : Q from pToQ : {P*, Q}
  return {arg1, pTo, q};         # return {arg1, pTo, q} : {[P, Q*], P*, Q}
};

let mp3 : MP = {
  intro arg1, q : Q;             # intro arg1 : Q* and q : Q
  intro arg2, p : P;             # intro arg2 : P* and p : P
  return {[p, arg1], arg2, q};   # return {[p, arg1], arg2, q} : {[P, Q*], P*, Q}
};
