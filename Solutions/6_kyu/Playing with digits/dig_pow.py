def dig_pow(n, p):
    d=[int(i) for i in list(str(n))]
    return -1 if sum([d[i]**(p+i) for i in range(len(d))])%n!=0 else sum([d[i]**(p+i) for i in range(len(d))])//n