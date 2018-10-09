MOD = 998244353
inverse=[0,1]
for i in range(2,80001):
    inverse.append(((i-inverse[MOD%i])*(MOD//i)+MOD%i)%MOD)

# p-field inverse (p==MOD which is prime)
# n*a[n]==1 (a[n] is the inverse of n) =>
# n*a[n]==1+k*p
# division: q=p//n, r=p%n
# n*a[n]==1+k*(q*n+r)
# 1==r*a[r] (r<n, induction) =>
# a[n]=k*q+r*(a[r]+k)/n =>
# k == n-a[r] (MOD p)
# we get the inverse of n

def height(n, m):  
    ret, trm = 0, 1
    m %= MOD
    for i in range(1, n + 1): 
        trm = trm * (m - i + 1) * inverse[i] % MOD
        ret = (ret + trm) % MOD
    return ret % MOD   