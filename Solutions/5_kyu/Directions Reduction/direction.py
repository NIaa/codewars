def dirReduc(arr):
    for i in range(len(arr)-1):
        if (arr[i]=="NORTH" and arr[i+1]=="SOUTH") or (arr[i]=="SOUTH" and arr[i+1]=="NORTH") or (arr[i]=="EAST" and arr[i+1]=="WEST") or (arr[i]=="WEST" and arr[i+1]=="EAST"):
            return dirReduc(arr[:i]+arr[i+2:])
    return arr