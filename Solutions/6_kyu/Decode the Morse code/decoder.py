def decodeMorse(morseCode):
    while " "==morseCode[0]:
        morseCode=morseCode[1:]
    if "   " in morseCode: 
        words=morseCode.split("   ")
    else: words=[morseCode]
    words=[word.split() for word in words]
    return " ".join(["".join([MORSE_CODE[i] for i in word]) for word in words])