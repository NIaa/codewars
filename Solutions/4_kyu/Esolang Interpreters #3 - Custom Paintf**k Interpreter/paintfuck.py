def interpreter(code, iterations, width, height):
    ret=[['0' for _ in range(width)] for _ in range(height)]
    code=[c for c in code if c in "news*[]"]
    it, p, x, y = 0, 0, 0, 0
    while it in range(iterations) and p in range(len(code)):
        if code[p]=='n': y=(y-1)%height
        if code[p]=='e': x=(x+1)%width
        if code[p]=='s': y=(y+1)%height
        if code[p]=='w': x=(x-1)%width
        if code[p]=='*': ret[y][x]={'1':'0', '0':'1'}[ret[y][x]]
        if code[p]=='[':
            if ret[y][x]=='0':
                layer=1
                while layer>0:
                    p+=1
                    if code[p]=='[': layer+=1
                    if code[p]==']': layer-=1
        if code[p]==']':
            if ret[y][x]!='0':
                layer=1
                while layer>0:
                    p-=1
                    if code[p]==']': layer+=1
                    if code[p]=='[': layer-=1  
        p+=1
        it+=1
        pass
    return '\r\n'.join([''.join(row) for row in ret])