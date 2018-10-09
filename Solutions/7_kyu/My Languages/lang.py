def my_languages(d):
    return [i for i in reversed(sorted(d, key=lambda x:d[x])) if d[i]>=60]