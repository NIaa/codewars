def move(p,direction):
    if direction==0:return [p[0]-1,p[1]]
    elif direction==1: return [p[0],p[1]+1]
    elif direction==2: return [p[0]+1,p[1]]
    else: return [p[0],p[1]-1]
def around(p,g):
    return g[p[0]-1][p[1]]+g[p[0]][p[1]+1]+g[p[0]+1][p[1]]+g[p[0]][p[1]-1]
def getChoice(p,directions,g):
    choice=0
    direction=[]
    for i in directions:
        if i%2:
            if around(p,g)[i] in ['-','+','X']: 
                choice+=1
                direction+=[i]
        else: 
            if around(p,g)[i] in ['|','+','X']: 
                choice+=1
                direction+=[i]
    return [choice,direction]
def line(grid):
    g=grid[:]
    for i in range(len(g)):
        g[i]=" "+g[i]+" "
    g.insert(0,len(g[0])*" ")
    g.append(len(g[0])*" ")
    m,n=len(g),len(g[0])
    ends=[[i,j]for i in range(m) for j in range(n) if g[i][j]=='X']
    if len(ends)!=2: return False
    for end in ends:
        p=end
        path=[p]
        choice=getChoice(p,range(4),g)
        if choice[0]==1:
            direction=choice[1][0]
            p=move(p,direction)
            path.append(p)
        else:break
        while 1:
            directions=[]
            if g[p[0]][p[1]]=="X":
                #return path
                for i in range(m):
                    for j in range(n):
                        if g[i][j]!=' ' and not [i,j] in path:
                            return False
                return True
            if g[p[0]][p[1]]=="+": directions=[(direction+1)%4,(direction-1)%4]
            if g[p[0]][p[1]]=="-" or g[p[0]][p[1]]=="|": 
                directions=[direction]
            choice=getChoice(p,directions,g)
            if choice[0]==1:
                direction=choice[1][0]
                p=move(p,direction)
                path.append(p)
            else:
                if choice[0]==2:
                    if (move(p,choice[1][0]) in path and not move(p,choice[1][1]) in path):
                        direction=choice[1][1]
                        p=move(p,direction)
                        path+=[p]
                        continue
                    if (move(p,choice[1][1]) in path and not move(p,choice[1][0]) in path):
                        direction=choice[1][0]
                        p=move(p,direction)
                        path+=[p]
                        continue
                #return p
                print(111111111)                
                #g[p[0]]=g[p[0]][:p[1]]+"?"+g[p[0]][p[1]+1:]
                for i in range(m):
                    print(g[i]+"1")
                #return choice
                break
#consider"XX"
    return False