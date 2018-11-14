def interpreter(c, t):
    code, tape = list(c), list(t)
    p_c, p_t = 0, 0
    while p_c in range(len(code)) and p_t in range(len(tape)):
        if code[p_c]=='>':
            p_t+=1
        if code[p_c]=='<':
            p_t-=1
        if code[p_c]=='*':
            tape[p_t]={'1':'0', '0':'1'}[tape[p_t]]
        if code[p_c]=='[':
            if tape[p_t]=='0':
                layer=1
                while layer>0:
                    p_c+=1
                    if code[p_c]=='[': layer+=1
                    if code[p_c]==']': layer-=1
            else: 
                p_c+=1
            continue
        if code[p_c]==']':
            if tape[p_t]!='0':
                layer=1
                while layer>0:
                    p_c-=1
                    if code[p_c]==']': layer+=1
                    if code[p_c]=='[': layer-=1                    
            else:
                p_c+=1
            continue
        p_c+=1
    return ''.join(tape)