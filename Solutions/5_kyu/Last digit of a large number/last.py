def last_digit(n1, n2):
    if n2==0: return 1
    last,p=n1%10,(n2%4-1)
    dict={2:[2,4,8,6],3:[3,9,7,1],4:[4,6,4,6],7:[7,9,3,1],8:[8,4,2,6],9:[9,1,9,1]}
    if last==0 or last==1 or last==5 or last==6:
        return last
    else:
        return dict[last][p]