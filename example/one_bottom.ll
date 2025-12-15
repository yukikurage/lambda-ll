let one : [] = {
  return [];
};

let oneAndBottom : {[], {}} = {
  return {[], {}};
};

let oneAndBottomBottom : {[], {}, {}} = {
  return {[], {}, {}};
};

let removeBottom : ({}) => {} = (bottom : {}) => {
  let {} = bottom;
  return {};
};


#- Should not compile
let bottom : {} = {
  return {};
};
-#

#- Should not compile
let removeOne : ([]) => {} = (one : []) => {
  let [] = one;
  return {};
};
-#


