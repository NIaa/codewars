def pwr(n)
    return [[1,0],[0,1]] if n==0
    return [[0,1],[1,1]] if n==1
    a=pwr(n/2)
    ret=[[a[0][0]**2+a[0][1]*a[1][0],a[0][1]*(a[0][0]+a[1][1])],
         [a[1][0]*(a[0][0]+a[1][1]),a[1][1]**2+a[0][1]*a[1][0]]]
    return ret if n%2==0
    return [[ret[0][1],ret[0][0]+ret[0][1]],[ret[1][1],ret[1][0]+ret[1][1]]]
  end
  def fib(n)
    ret=pwr(n.abs)[1][0]
    return (n>0||(n%2==1)) ? ret : -ret
  end