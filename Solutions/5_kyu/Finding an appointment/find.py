def get_start_time(schedules, duration):
    # O(n^2)

    l=[[0,540]]
    for man in schedules:
        for x in man:
            l+=[[60*(int(x[0][:2]))+(int(x[0][3:5])),60*(int(x[1][:2]))+(int(x[1][3:5]))]]
    l.sort()
    # Merge businessmen's schedules ,convert them into minutes and sort.
    
    i,j=0,0
    while i<len(l):
        j=i+1
        while j<len(l):
            if l[i][1]>l[j][0]:
                l[i]=[l[i][0], max(l[i][1],l[j][1])]
                l.pop(j)
                j-=1
            j+=1
        i+=1
    # Deal with overlapping schedules.
    
    for i in range(len(l)-1):
        if i!=len(l) and l[i][1]+duration<=l[i+1][0] and l[i][1]+duration<=19*60:
            return str(l[i][1]//60//10)+str(l[i][1]//60%10)+':'+str(l[i][1]%60//10)+str(l[i][1]%60%10)
    if l[-1][1]+duration<=19*60:
        return str(l[-1][1]//60//10)+str(l[-1][1]//60%10)+':'+str(l[-1][1]%60//10)+str(l[-1][1]%60%10)
    return None            
    # Finding an appointment