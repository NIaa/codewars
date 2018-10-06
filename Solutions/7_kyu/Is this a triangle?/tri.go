package kata

func IsTriangle(a, b, c int) bool {
  return a+b>c && b+c>a && c+a>b
}