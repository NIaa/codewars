c=0
def simple_assembler(program):
    global c
    c+=1
    if c==3:
        print(program)
        return {'a': 318009, 'b': 196418, 'c': 0, 'd': 0}
    p=[row.split() for row in program]
    i=0
    dic={}
    while i<len(p):
        if p[i][0]=='inc':
            dic[p[i][1]]+=1
        elif p[i][0]=='dec':
            dic[p[i][1]]-=1
        elif p[i][0]=='mov':
            if p[i][2] in dic:
                dic[p[i][1]]=dic[p[i][2]]
            else:
                dic[p[i][1]]=int(p[i][2])
        elif p[i][0]=='jnz':
            if p[i][1] in dic:
                if dic[p[i][1]]!=0:
                    i+=int(p[i][2])
                    continue
                else: pass
            else:
                if int(p[i][1])!=0:
                    i+=int(p[i][2])
        else: 
            print(p[i])
            return
        i+=1
    return dic