def differentiate(eq, x):
    l ,d ,p = [], {}, 0
    for i in range(len(eq)):
        if i==0 and eq[i]=='-': continue
        elif eq[i] == '+' or eq[i] == '-':
            l.append(eq[p:i]); p=i
    l.append(eq[p:])
    for i in l:
        c_i, e_i = i.find('x'), i.find('^')
        if c_i == -1: cof = int(i)
        elif c_i == 0 or (c_i == 1 and i[0] == '+') : cof = 1
        elif c_i == 1 and i[0] == '-': cof = -1
        else: cof = int(i[:c_i])        
        if e_i == -1:
            if c_i == -1: exp = 0
            else: exp = 1
        else: exp = int(i[e_i+1:])
        d[exp] = cof
    l=[]
    for i in d:
        if i!=0: l.append((i*d[i],i-1))
        else: l.append((0,0))
    print(eq)
    return sum([i[0]*x**i[1] for i in l])