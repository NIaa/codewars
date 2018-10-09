int max(int a, int b) { return (a>b)?a:b; }
int expression_matter(int a, int b, int c) {
  return max(max(max(max(max(a+b+c,a+b*c),a*b+c),a*b*c),(a+b)*c),a*(b+c));
}