def base64_to_base10(str):
    BASE="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    return sum([64**j*([BASE.index(i) for i in str][::-1])[j] for j in range(len(str))])