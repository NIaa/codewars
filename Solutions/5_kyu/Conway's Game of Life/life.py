def next_gen(cells):
    if cells==[]: return cells
    m,n=len(cells),len(cells[0])
    c=[[0 for _ in range(2+n)]]+[[0]+i+[0] for i in cells]+[[0 for _ in range(2+n)]]
    next=[[0 for _ in range(2+n)] for _ in range(2+m)]
    for i in range(m):
        for j in range(n):
            neighbors=c[i][j]+c[i][j+1]+c[i][j+2]+c[i+1][j]+c[i+1][j+2]+c[i+2][j]+c[i+2][j+1]+c[i+2][j+2]
            if c[i+1][j+1]==1:
                if neighbors<2 or neighbors>3: next[i+1][j+1]=0
                else: next[i+1][j+1]=1
            else:
                if neighbors==3: next[i+1][j+1]=1
                else: next[i+1][j+1]=0
    return [[next[i+1][j+1] for j in range(n)] for i in range(m)]
