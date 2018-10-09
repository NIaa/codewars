import math
def listPosition(word):
    dic=[0 for i in range(26)]
    for c in word:
        dic[ord(c)-ord('A')]+=1
    ret=0
    sz=len(word)
    for i in range(sz):
        small=dic[:(ord(word[i])-ord('A'))]
        term=factorial(sz-i-1)*sum(small)
        for j in dic:
            term/=factorial(j)
        dic[ord(word[i])-ord('A')]-=1
        ret+=term
    return ret+1