def maxSequence(arr):
    max_here, max_all = 0, 0
    for i in arr:
        max_here=max(max_here+i, 0)
        max_all=max(max_all, max_here)
    return max_all