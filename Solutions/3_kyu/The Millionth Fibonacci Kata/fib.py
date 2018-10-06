def power(n):
    if n==0: return [[1,0],[0,1]]
    if n==1: return [[0,1],[1,1]]
    a=power(n//2)
    ret=[[a[0][0]**2+a[0][1]*a[1][0],a[0][1]*(a[0][0]+a[1][1])],\
        [a[1][0]*(a[0][0]+a[1][1]),a[1][1]**2+a[0][1]*a[1][0]]]
    return [[ret[0][1],ret[0][0]+ret[0][1]],[ret[1][1],ret[1][0]+ret[1][1]]] if n%2 else ret
def fib(n):
    ret=power(abs(n))[1][0]
    if n>0: return ret
    return ret if n%2 else -ret