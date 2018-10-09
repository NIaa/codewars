def valid_parentheses(string):
    left = right = 0
    for c in string:
        if c == '(': left+=1
        if c == ')': right+=1
        if right > left: return False
    return left == right