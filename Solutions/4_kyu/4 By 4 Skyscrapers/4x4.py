import itertools
def diff(l1,l2):
    for i in range(len(l1)):
        if l1[i]==l2[i]:
            return False
    return True
def see(l):
    ret=[l[0]]
    for i in range(4):
        if l[i]>ret[-1]:
            ret.append(l[i])
    return len(ret)
def get_clue(m):
    ret=[]
    ret.append(see([m[0][0],m[1][0],m[2][0],m[3][0]]))
    ret.append(see([m[0][1],m[1][1],m[2][1],m[3][1]]))
    ret.append(see([m[0][2],m[1][2],m[2][2],m[3][2]]))
    ret.append(see([m[0][3],m[1][3],m[2][3],m[3][3]]))
    ret.append(see(m[0][::-1]))
    ret.append(see(m[1][::-1]))
    ret.append(see(m[2][::-1]))
    ret.append(see(m[3][::-1]))
    ret.append(see([m[3][3],m[2][3],m[1][3],m[0][3]]))
    ret.append(see([m[3][2],m[2][2],m[1][2],m[0][2]]))
    ret.append(see([m[3][1],m[2][1],m[1][1],m[0][1]]))
    ret.append(see([m[3][0],m[2][0],m[1][0],m[0][0]]))
    ret.append(see(m[3]))
    ret.append(see(m[2]))
    ret.append(see(m[1]))
    ret.append(see(m[0]))
    return ret
def solve_puzzle (clues):
    lst,latin=[],[]
    for i in itertools.permutations([1,2,3,4],4):
        lst.append(i)
    for i in lst:
        for j in lst: 
            if diff(i,j): 
                for k in lst: 
                    if diff(i,k) and diff(j,k):
                        for l in lst: 
                            if diff(i,l) and diff(j,l) and diff(k,l):
                                latin.append((i,j,k,l))
    for m in latin:
        if [get_clue(m)[i] if clues[i]!=0 else 0 for i in range(16)]==list(clues): return m