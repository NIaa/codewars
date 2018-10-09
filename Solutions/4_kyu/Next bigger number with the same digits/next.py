def next_bigger(n):
    if list(str(n))==sorted(str(n),reverse=True):
        return -1
    l=[int(i) for i in list(str(n))]
    for i in range(len(l))[::-1]:
        if l[i-1]<l[i]:
            for j in range(i,len(l))[::-1]:
                if l[j]>l[i-1]:
                    l[i-1],l[j]=l[j],l[i-1]
                    break
            return int("".join(([str(l[x]) for x in range(i)])+sorted([str(l[x]) for x in range(i,len(l))])))