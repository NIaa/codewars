def last_digit(lst):
    dict={                  0:[0,0,0,0],1:[1,1,1,1],
    2:[2,4,8,6],3:[3,9,7,1],4:[4,6,4,6],5:[5,5,5,5],
    6:[6,6,6,6],7:[7,9,3,1],8:[8,4,2,6],9:[9,1,9,1]}
    copy=lst[:]
    if 0 in copy:
        count=0
        for i in range(copy.index(0),len(copy)):
            if copy[i]==0: count+=1
            else: break
        copy=copy[0:copy.index(0)+count%2]
    if copy==[]: return 1
    first=copy.pop(0)%10
    if copy==[]: return first
    if copy[0]==0: return 1
    second=copy.pop(0)%4
    if 0 in copy:
        if copy==[0]:
            return first
        else:
            copy.pop()
            copy[-1]=1     
    if copy==[]: return dict[first][second-1]
    if second==0: return dict[first][3]
    elif second==1: return first
    elif second==2:
        if len(copy)==0 or copy[0]==1: return dict[first][1]
        else: return dict[first][3]
    else:
        if copy[0]%2: return dict[first][2]
        else: return first