def anagrams(word, words):
    word = ''.join(sorted(word))
    list = []
    for w in words:
        buf = ''.join(sorted(w))
        if buf == word:
            list.append(w)
    return list