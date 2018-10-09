def validateBattlefield(field):
    f=field[:]

    for i in range(10): 
        f[i].insert(0,0)
        f[i].append(0)
    f.insert(0,[0 for i in range(12)])
    f.append([0 for i in range(12)])

    points=0
    for i in range(1,11):
        for j in range(1,11):
            if f[i][j]==1:
                if f[i+1][j+1]==1 or f[i+1][j-1]==1: return False#Corner case
                points+=1
    if points!=20: return False
    
    ship=[0,0,0,0]#1234
    for i in range(1,11):
        for j in range(1,11):
            if f[i][j]==1:
                if f[i-1][j]==0 and f[i][j-1]==0:
                    m=n=0
                    while f[i+m][j]!=0: m+=1
                    while f[i][j+n]!=0: n+=1
                    if [m,n]==[0,1]: ship[0]+=1
                    elif [m,n]==[1,1]: ship[0]+=1
                    elif [m,n]==[2,1] or [m,n]==[1,2]: ship[1]+=1
                    elif [m,n]==[3,1] or [m,n]==[1,3]: ship[2]+=1
                    elif [m,n]==[4,1] or [m,n]==[1,4]: ship[3]+=1
                    else: return False
    return ship==[4,3,2,1]    