def sudoku(puzzle):
    log=[[[] for y in range (9)] for x in range(9)]
    for x in range(9):
        for y in range(9):
            if puzzle[y][x]!=0: log[y][x]=[puzzle[y][x]]
            else: log[y][x]=[i for i in range(1,10)]
    for cycle in range(6):#or check if all len()==1
        for x in range(9):
            for y in range(9):
                if len(log[y][x])==1:
                    for a in range(9):
                        if log[y][x][0] in log[y][a] and a!=x:  
                            log[y][a].remove(log[y][x][0])
                    for b in range(9):
                        if log[y][x][0] in log[b][x] and b!=y:
                            log[b][x].remove(log[y][x][0])
                    box=y//3*3+x//3
                    for a in range(3):
                        for b in range(3):
                            if log[y][x][0] in log[box//3*3+b][box%3*3+a] and x!=box%3*3+a and y != box//3*3+b:
                                log[box//3*3+b][box%3*3+a].remove(log[y][x][0])
    return [[log[y][x][0] for x in range(9)]for y in range(9)]