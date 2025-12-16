#-
Types: P, Q,..., ~P, ~Q,... [P,...], {P,...},

Inverse: 
~P = ~P
~[P, Q,...] = {~P, ~Q,...}
~{P, Q,...} = [~P, ~Q,...]
-#

# (A, B) => C is syntax suger for {A*, B*, C}
# (P => Q, P) => Q 
#   = {~(P => Q), ~P, Q}
#   = {~{~P, Q}, ~P, Q}
#   = {[P, ~Q], ~P, Q} 
type MP = (P => Q, P) => Q;

let mp1 : MP = {
  # introduce variables of types ~A and A
  # arg1: ~(P => Q) = ~{~P, Q} = [P, ~Q]
  intro arg1, pToQ : P => Q; 
  # arg2: ~P, p : P
  intro arg2, p : P;
  # destruct pToQ : {~P, Q} into pInv : ~P and q : Q 
  let {pInv, q} = pToQ;
  # eliminate terms of types ~A and A
  # here, p : P and pInv : ~P
  elim p, pInv;
  # return {arg1, arg2, q} : {[P, ~Q], ~P, Q} = MP
  return {arg1, arg2, q};
};

# with syntax suger
# (x, y) => t is syntax suger for {intro _x, x; intro _y, y; return {_x, _y, t}}
# f(t, u) is syntax suger for 
#   {
#     {_arg1, _arg2, _result} = f;
#     elim _arg1, t; 
#     elim _arg2, u;
#     return _result;
#   } 
let mp2 : MP = (f : P => Q, x : P) => f(x);

type Inner = [P, {Q, R}] => {Q, [P, R]};

let inner : Inner = ([p : P, {q : Q, r : R}]) => {q, [p, r]};

let excludedMiddle : {P, ~P} = {intro pInv, p : P; return {p, pInv}};

let excludedMiddle' : {P, P => {}} = {
  intro pInv, p : P;
  return {
    p, 
    (argP : P) => {
      elim argP, pInv; 
      return {};
    }
  };
};

# block pattern destructing
let blockPat = (p : P, q : Q) => {
  let 
    { 
      require [x, y];
      intro a, b : P;
      elim x, a;
    } = [p, q];
  return [b, y];
};

let blockPatLambda = ({require x : P; let y = (f : P => Q) => f(x);}) => y;