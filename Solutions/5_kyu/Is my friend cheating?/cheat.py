def removNb(n):
    ret=[]
    sum=n*(n+1)//2
    for i in range(1,n+1):
        if (sum+1)%(i+1)==0:
            j=(sum+1)//(i+1)-1
            if j in range(1,n+1):
                ret.append((i,j))  
    return ret            