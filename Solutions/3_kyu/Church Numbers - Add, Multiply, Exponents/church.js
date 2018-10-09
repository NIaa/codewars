var churchAdd = (c1) => (c2) => (f) => (x) => c1(f)(c2(f)(x));
var churchMul = (c1) => (c2) => (f) => c1(c2(f));
var churchPow = (cb) => (ce) => ce(cb);