def generate_hashtag(s):
    ret=s.split()
    if not ret: return False
    for i in range(len(ret)):
        ret[i]=ret[i][0].upper()+ret[i][1:].lower()
    ret="#"+"".join(ret)
    return ret if len(ret)<=140 else False