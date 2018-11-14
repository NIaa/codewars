function genfib(){
  f = [0, 1]
  return function fib(){
    f[f.length] = f[f.length-1] + f[f.length-2]
    return f[f.length-3]
  }
}