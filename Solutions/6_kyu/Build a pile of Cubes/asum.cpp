class ASum
{
  public:
  static long long findNb(long long m)
  {
    long n=0;
    while(n*n*(n+1)*(n+1)/4<m) n++;
    return (n*n*(n+1)*(n+1)/4==m)?n:-1;
  }
};