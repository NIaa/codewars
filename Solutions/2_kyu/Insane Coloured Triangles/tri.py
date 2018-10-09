# Let R,G,B=0,1,2
# for x y 
#      z  
# notice that z==(2*(x+y)) (Mod 3)
#
# a        b        c
#   2(a+b)   2(b+c)
#      4(a+2b+c)
#
# a      b         c        d
#  2(a+b)   2(b+c)   2(c+d)
#    4(a+2b+c) 4(b+2c+d)
#      8(a+3b+3c+d)
#
# Using induction, we can prove the result of 
# x0 x1 x2 ... xn    is
# 2^n*(Sum((n,k)*xk))
# where (n,k) is binomial coefficient
# As a result,
# if n%3==0, any k, (n,k)%3==0

def triangle(row):
    reduce=[3**i+1 for i in range(10) if 3**i<=100000][::-1]
    for length in reduce:
        while len(row)>=length:
            row=[row[i] if row[i]==row[i+length-1] else ({"R","G","B"}-{row[i],row[i+length-1]}).pop() for i in range(len(row)-length+1)]
    return row[0]