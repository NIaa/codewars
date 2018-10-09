def validBraces(string):
    stack=[]
    for c in string:
        if c=='(' or c=='[' or c=='{':
            stack+=[c]
        elif c==')':
            if stack==[] or stack[-1]!='(': return False
            else: stack.pop()
        elif c==']':
            if stack==[] or stack[-1]!='[': return False
            else: stack.pop()
        elif c=='}':
            if stack==[] or stack[-1]!='{': return False
            else: stack.pop()
    return True if stack==[] else False