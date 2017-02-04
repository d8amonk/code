def censor(text, word):
    texts = str(text)
    words = texts.split() #creates the list
    x = "" * len(word) #creates the stars with correct length
    for i in range(len(words)):
        if words[i] == word: 
            words[i] = stars #replace
    return "".join(words)

# does the same thing, more compact code -> better
# def censor(text, word):
#     return text.replace(word, "*" * len(word))