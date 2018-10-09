def longest_slide_down(p):
    l=p[-1]
    while len(l)!=1:
        l=[(p[len(l)-2][i]+max(l[i],l[i+1])) for i in range(len(l)-1)]
    return l[0]