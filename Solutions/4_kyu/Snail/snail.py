def snail(a):
    ret=[]
    for i in range(2*len(a)-1):
        if i%4==0:
            ret+=a.pop(0)
        elif i%4==1:
            for j in range(len(a)):
                ret.append(a[j].pop())
        elif i%4==2:
            ret+=reversed(a.pop())
        else:
            for j in range(len(a))[::-1]:
                ret.append(a[j].pop(0))
    return ret