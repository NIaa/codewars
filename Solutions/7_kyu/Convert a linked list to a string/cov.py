def stringify(list):
    ret=""
    while list!=None:
        ret+=str(list.data)+" -> "
        list=list.next
    ret+="None"
    return ret
     