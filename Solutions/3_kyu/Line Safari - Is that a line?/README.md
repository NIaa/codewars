<https://www.codewars.com/kata/line-safari-is-that-a-line>
<b><b>
```python
def line(grid):
    g = {(r, c):v for r, row in enumerate(grid) for c, v in enumerate(row) if v.strip()}
    ends = [k for k in g if g[k] == 'X']
    if len(ends) != 2: return False
    
    for start, finish in [ends, ends[::-1]]:
        path = [start]
        while path[-1] != finish:
            r, c = path[-1]            
            d, V, H = g[path[-1]], [(r+1 ,c), (r-1, c)], [(r, c-1), (r, c+1)]
            moves = {'+':V if len(path) > 1 and path[-1][0] == path[-2][0] else H, '|':V, '-':H, 'X':H+V}[d]
            possibles = {p for p in moves if p in g and p not in path and (d == '+' or (p[0] == r and g[p] != '|') or (p[1] == c and g[p] != '-'))}
            
            if len(possibles) != 1: break
            path.append(possibles.pop())
        if len(g) == len(path): return True        
    return False
```