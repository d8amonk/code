def censor_word(word, word_to_censor):
    word = word.strip()
    if word.lower() == word_to_censor.lower():
        return '*' * len(word)
    else:
        return word

def censor(line, word_to_censor):
    words = str(line).split(',') #creates the list
    words = [censor_word(w, word_to_censor) for w in words]
    return ", ".join(words)

with open("test.csv", "rt") as f:
    for line in f:
        print(censor(line, "USA"))