def my_first_interpreter(code):
    # Make your esolang interpreter here
    ret=""
    p=0
    for i in code:
        if i=='+': 
            p+=1
            p%=256
        if i=='.':
            ret+=chr(p)
    return ret