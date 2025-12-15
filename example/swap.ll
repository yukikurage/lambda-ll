#-
Tensor Swap: [P, Q] => [Q, P]
-#

type Swap = [P, Q] => [Q, P];

let swap : Swap = (pair : [P, Q]) => {
    let [p, q] = pair;
    return [q, p];
};
