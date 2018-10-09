import math  
def list_squared(m, n):
    def isSquare(x):
        a=math.sqrt(x)
        return a==int(a)
    def sumFactor2(x):
        sum = 0
        for i in range(1,int(math.sqrt(x)+1)):
            if not x%i:
                sum+=i*i
                if x//i!=i:
                    sum+=x//i*x//i                     
        return sum
    sf2=0
    ret=[]
    for i in range(m,n+1):
        sf2=sumFactor2(i)
        if(isSquare(sf2)):
            ret.append([i,sf2])
    return ret