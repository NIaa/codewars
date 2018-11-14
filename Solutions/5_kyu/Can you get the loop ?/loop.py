def loop_size(node):
    lst=[node]
    while True:
        tmp=lst[-1]
        if tmp.next in lst: 
            lst.append(tmp.next)
            break
        else: lst.append(tmp.next)
    return len(lst)-lst.index(lst[-1])-1