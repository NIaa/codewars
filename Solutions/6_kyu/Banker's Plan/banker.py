def fortune(f0, p, c0, n, i):
    for year in range(n-1):
        f0=int(f0*(1+p*0.01)-c0)
        c0=int(c0*(1+i*0.01))
    return f0>=0