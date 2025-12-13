★Create parser & type checker

- take string
- parse into AST
- chck type
- for now, implement "tensor" / "par" feature, that is, (t, ..., t) intro & elim, {t, ..., t} intro & elim, statement, let, intro x, x, elim t, t, <- , ->, (type : tensor, par, prim type, type inverse)
- should successflly parse & check example/basic.ll
- ref doc/type-system.md

You can create type checker by following strategy:

- create
  - check_term: Term -> ContextTree × Variables -> ContextTree × Variables
  - check_statement: Statement -> ContextTree × Variables -> ContextTree × Variables
- when checking {s;s;... ;return t},...
- create empty context tree (if no context tree is provided)
  - for nested scope, reuse context tree
  - should memoize the variables that are introduced int the scope (will be used at type check of `return t`)
- read statements in by normal order
- check right hand term: (will recursivelly change context tree, use connectNode & Variables to make structured data type e.g. `(x, y)` connects varId("x") and varId("y") by "tensor". `({x, y}, z)` connects varId("x") and varId("y") by "par", then connect that node to varId("z") by "tensor")
  - you should create multi-connect (multi means >=1) function in contextTree.ts, that call connect function N - 1 times, then flatten nested types: (((x, y), z), w) -> (x, y, z, w)
  - for now, `()` & `{}` are not allowed (so at least one element should be in tensor/par)
- check statements:
  - let x (: T) = t;
    - check t, then get node id of t
    - add variable x that indicates node id of t
    - if type T is provided, then assume T === type of context node of t
  - let (x1, ... , xn) = t;
    - check t, then get node id of t
    - check type of context node of t is tensor, then
    - call destructNode to get ids of (x1, ... , xn)
    - add variables (x1, ... , xn) that indicates node ids of (x1, ... , xn)
    - you should assume the length of (x1, ... , xn) is same as the length of element of type of t
  - let {x1, ... , xn} = t;
    - check t, then get node id of t
    - check type of context node of t is par, then
    - call destructNode to get ids of (x1, ... , xn)
    - add variables (x1, ... , xn) that indicates node ids of (x1, ... , xn)
    - you should assume the length of (x1, ... , xn) is same as the length of element of type of t
  - intro x, y : T;
    - add tensor root node (N1) and under it add par node (N2)
    - add variable x with type T\* to N2
    - add variable y with type T to N2
    - e.g. Tensor [x, y] -> Tensor[Par[x: T*, y: T], Tensor[x, y]]
  - elim t1, t2;
    - check t1, then get node id of t1
    - check t2, then get node id of t2
    - check inverse of type of t1 is same as type of t2
    - connect t1 & t2 by "tensor" (if possible) -> get id of connected node
    - then remove connected node
  - return t;
    - must be the last statement ({s;s;... ;return t})
    - check t, then get node id of t
    - type of ({s; s; ...; return t}) will be same as type of t
    - assume that no node exept for t and outer variables is left in context tree (after normalize, it should be Par [t])

★Create web app

- use Next & Monaco Editor to implement
- type checking should be done in 1 second
- type error should be displayed
- show top level variable's types (it means "proof is done")
- (advanced feature) show proof net
  - this is not context tree itself; more easy one! (because connect doesn't move nodes, just add edges)
  - top-down graph (split & merge)
  - term t -> 適宜変数を繋ぐ
  - intro x, y -> 上に凸な曲線、半円みたいな感じ ノードが頂点にある　それぞれの足を x, y と名付ける
  - elim t1, t2 -> t1 と t2 を作ったらそれを下に凸な曲線でつなぐ
  - let x = t -> t を作って x に rename
  - let (x1, ... , xn) = t -> t を作って、x1,... , xn に分割
  - let {x1, ... , xn} = t -> t を作って、x1, ... , xn に分割
  - (t, ...) -> ノードから線を出してつなぐ
  - {t, ...} -> ノードから線を出してつなぐ
  - {s; s; ... ; return t} -> sub proof net の描画を行う
