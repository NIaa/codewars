int sumTriangularNumbers(int n) {
  return n<0?0:(2+n)*(1+n)*n/6;
}