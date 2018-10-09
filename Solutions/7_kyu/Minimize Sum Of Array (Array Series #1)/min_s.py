def min_sum(arr):
    if len(arr)==0: return 0
    t=max(arr)*min(arr)
    arr.pop(arr.index(max(arr)))
    arr.pop(arr.index(min(arr)))   
    return t+min_sum(arr)