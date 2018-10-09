from fractions import Fraction
def bernoulli_number(n):
    if n%2!=0 and n!=1: return 0
    b=[1]
    pascal=[[1],[1,1]]
    for i in range(n):
        pascal.append([1]+[pascal[i+1][j]+pascal[i+1][j+1] for j in range(i+1)]+[1])
        b+=[Fraction(-sum([pascal[i+2][j]*b[j] for j in range(i+1)]),i+2)]
    return b[-1]