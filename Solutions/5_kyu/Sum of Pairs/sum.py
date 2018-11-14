def sum_pairs(ints, s):
    a = sorted(ints)
    l,r,sum,ret=0,len(a)-1,None,[]
    while(1):
        if l==r: break
        sum=a[l]+a[r]
        if sum==s:
            ret.append([a[l],a[r]])
            r-=1
        elif sum<s: l+=1
        else: r-=1
    if ret==[]: return None
    result,v,v_min = ret[0],len(ints)-1,len(ints)-1
    for pair in ret:
        if pair[0]==pair[1]: v=[i for i in ints if i == pair[0]][1]
        else: v=max(ints.index(pair[0]),ints.index(pair[1]))
        if v<=v_min:
            v_min=v
            result=[ints[min(ints.index(pair[0]),ints.index(pair[1]))],ints[max(ints.index(pair[0]),ints.index(pair[1]))]]
    return result