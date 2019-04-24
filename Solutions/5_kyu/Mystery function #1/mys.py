def solved(string):
    l = list(string)
    return "".join(sorted(l[:len(l)//2] + l[len(l)//2+len(l)%2:]))