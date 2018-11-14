def get_generation(cells, generations):
    m,n=len(cells),len(cells[0])
    if generations==0: return cells
    c=cells[:]
    for _ in range(generations):
        buf=[[0 for _ in range(n)] for _ in range(m)]
        for i in range(m):
            for j in range(n):
                neighbors=c[(i-1)%m][(j-1)%n]+c[(i-1)%m][j%n]+c[(i-1)%m][(j+1)%n]+c[i][(j-1)%n]+c[i][(j+1)%n]+c[(i+1)%m][(j-1)%n]+c[(i+1)%m][j]+c[(i+1)%m][(j+1)%n]
                if c[i][j]==1:
                    if neighbors<2 or neighbors>3: buf[i][j]=0
                    else: buf[i][j]=1
                else:
                    if neighbors==3: buf[i][j]=1
                    else: buf[i][j]=0
        c=buf
    return c 