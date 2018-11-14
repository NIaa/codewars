def domain_name(url):
    tails=[".com",".ru"]
    tail=min([url.index(t) for t in [s for s in tails if s in url]])
    for i in range(tail)[::-1]:
        if url[i] == '.' or url[i]=='/':
            head=i+1
            break
        if i==0: head=i
    print(url)
    return url[head:tail]