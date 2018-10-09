def total_inc_dec(x):
    def CnkD(n,k):
        from collections import defaultdict
        C=defaultdict(int)  
        for row in range(n+1):  
            C[row,0]=1  
            for col in range(1,k+1):  
                if col <= row:  
                    C[row,col]=C[row-1,col-1]+C[row-1,col]  
        return C[n,k]  
    if x==0: return 1
    if x==1: return 10
    if x==2: return 100
    return total_inc_dec(x-1)-10+CnkD(x+9,9)+CnkD(x+8,8)