def min_path(grid, x, y):
#  Matrix like    can be divided into     1       2        3
#  o o o o o o    3 parts like:         o o o    o o o    
#  o o o o o o    It's similar          o o    o o o       o
#  o o o o o o    if x<y                o    o o o       o o
    x+=1
    y+=1                                                #I am used to use lengh of the list
    g=[[grid[b][a] for a in range(x)]for b in range(y)] #just copy
    l=[g[0][0]]
    for i in range(min(x,y)-1):                         # 1 solve the upper left trangle
        if i==0: 
            l=[l[0]+grid[1][0], l[0]+grid[0][1]]
            continue
        l=[g[i+1][0]+l[0]]+[g[i-j][j+1]+min(l[j],l[j+1]) for j in range(i)]+[g[0][i+1]+l[-1]]
    if x>y:                                             # 2 solve the middle part
        for i in range(min(x,y), max(x,y)):       
            l=[g[y-j-1][i-y+j+1]+min(l[j],l[j+1]) for j in range(y-1)]+[g[0][i]+l[-1]]
    elif y>x:
        for i in range(min(x,y), max(x,y)):       
            l=[g[i][0]+l[0]]+[g[i-j-1][j+1]+min(l[j],l[j+1]) for j in range(x-1)]
    while(len(l))!=1:                                   # 3 solve the lower down trangle
        l=[g[y-i-1][x+i-len(l)+1]+min(l[i],l[i+1]) for i in range(len(l)-1)]
    return l[0]