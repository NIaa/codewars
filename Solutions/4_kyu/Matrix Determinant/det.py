def determinant(matrix):
    if len(matrix)==0: return 0
    if len(matrix)==1: return matrix[0][0]
    s=0
    for i in range(len(matrix)):
        s+=(-1)**i*matrix[0][i]*determinant([[matrix[m][n] for n in [x for x in range(len(matrix)) if x!=i]] for m in range(1,len(matrix))])
    return s