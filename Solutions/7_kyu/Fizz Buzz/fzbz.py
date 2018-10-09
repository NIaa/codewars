def solution(number):
    ret=[0,0,0]
    for i in range(1,number):
        if i%3==0 and i%5==0: ret[2]+=1
        elif i%3==0: ret[0]+=1
        elif i%5==0: ret[1]+=1
    return ret