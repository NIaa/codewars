def generate_diagonal(n, l):
    pascal=[[1]]
    for i in range(1,n+l):
        pascal.append([1]+[pascal[i-1][j]+pascal[i-1][j+1] for j in range(i-1)]+[1])
    return [pascal[n+i][n] for i in range(l)]