def count(n):
    l=len(bin(n)[2:])
    s=0
    for i in range(l):
        s+=(n+1)//(2**(i+1))*2**i+(n+1)%(2**(i+1))%2**i*((n+1)%2**(i+1)//(2**i))
    return s
def countOnes(left, right):
    return count(right)-count(left-1)