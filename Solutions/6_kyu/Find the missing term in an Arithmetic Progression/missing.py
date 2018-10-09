def find_missing(sequence):
    d=sequence[1]-sequence[0] if sequence[1]-sequence[0]==sequence[2]-sequence[1] else sequence[4]-sequence[3]
    for i in range(0,len(sequence)):
        if sequence[i+1]-sequence[i]!=d:
            return sequence[i]+d