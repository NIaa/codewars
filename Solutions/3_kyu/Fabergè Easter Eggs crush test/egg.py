def height(n, m):  
    ret=0
    trm=1       #every term
    for i in range(n):
        trm=trm*(m-i)//(i+1)
        ret+=trm
    return ret