BASE="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
def TRUE_to_base_64(string):
#'=' characters might be added to make the last block contain four base64 characters.
    bi=[bin(ord(i))[2:] for i in string]
    for i in range(len(bi)):
        bi[i]=(8-len(bi[i]))*"0"+bi[i]
    b="".join(bi)
    while len(b)%6!=0:
        b+='0'
    b64=[int(b[i*6:(i+1)*6],2) for i in range(len(b)//6)]
    ret= "".join(BASE[i] for i in b64)
    while len(ret)%4!=0:
        ret+='='
    return ret
def to_base_64(string):
    ret=TRUE_to_base_64(string)
    while ret[-1]=='=': ret=ret[:-1]
    return ret
def from_base_64(string):
    l=[bin(BASE.index(i))[2:] for i in string if i!='=']
    for i in range(len(l)):
        l[i]=(6-len(l[i]))*"0"+l[i]    
    b="".join(l)    
    ansc=[int(b[i*8:(i+1)*8],2) for i in range(len(b)//8)]
    return "".join([chr(i) for i in ansc])