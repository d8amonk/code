def anti_vowel(text):
    v = "" #not []!!! (check to see why)
   
    for c in text:
        if c not in 'aeiouAEIOU':
            v += c
    return v