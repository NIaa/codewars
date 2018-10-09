def solution(n):
    d1={0: '', 1:'I', 2:'II', 3:'III', 4:'IV', 5:'V', 6:'VI', 7:'VII', 8:'VIII', 9:'IX'}
    d2={0: '', 1:'X', 2:'XX', 3:'XXX', 4:'XL', 5:'L', 6:'LX', 7:'LXX', 8:'LXXX', 9:'XC'}
    d3={0: '', 1:'C', 2:'CC', 3:'CCC', 4:'CD', 5:'D', 6:'DC', 7:'DCC', 8:'DCCC', 9:'CM'}
    ret=""
    ret+=n//1000*'M'
    n%=1000
    ret+=d3[n//100]
    n%=100
    ret+=d2[n//10]
    n%=10
    ret+=d1[n]
    return ret