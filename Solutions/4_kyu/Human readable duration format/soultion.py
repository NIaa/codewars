def format_duration(seconds):
    s=seconds%60
    m=(seconds//60)%60
    h=(seconds//3600)%24
    d=(seconds//86400)%365
    y=seconds//(86400*365)
    ret=""
    if seconds==0: return "now"
    if y!=0:
        ret+=str(y)+" year"
        if y!=1: ret+="s"
        ret+=", "
    if d!=0:
        ret+=str(d)+" day"
        if d!=1: ret+="s"
        ret+=", "
    if h!=0: 
        ret+=str(h)+" hour"
        if h!=1: ret+="s"
        if m!=0: 
            if s==0: ret+=" and "
            else: ret+=", "
    if m!=0:
        ret+=str(m)+" minute"
        if m!=1: ret+="s"
        if s!=0: ret+=" and "
    if s!=0:
        ret+=str(s)+" second"
        if s!=1: ret+="s"
    return ret