import math
def nearest_sq(n):
    if int(math.sqrt(n))**2==n: return n
    else:
        n1=int(math.sqrt(n))**2
        n2=int(1+math.sqrt(n))**2
        return n1 if n-n1<n2-n else n2