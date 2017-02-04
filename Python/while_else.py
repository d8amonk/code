from random import randint

# Generates a number from 1 through 10 inclusive
random_number = randint(1, 10)
print random_number #debugging

guesses_left = 3
# Start your game!

while guesses_left > 0:
    guess = int(raw_input("Your guess: "))
    if guess == random_number:
        print "You win!"
        break
    guesses_left -= 1
else:
    print "You lose."

#hobby list 
hobbies = []
for i in range(3):
    ans = raw_input("Name a hobby")
    hobbies.append(ans) 

#conditionally mark indices
phrase = "A bird in the hand..."

for char in phrase:
    if char == 'A' or char == 'a':
        print 'X',
    else:
        print char,

#basic dict
d = {'a': 'apple', 'b': 'berry', 'c': 'cherry'}

for key in d:
    print key, d[key]

#menu selection
choices = ['pizza', 'pasta', 'salad', 'nachos']

print 'Your choices are:'
for index, item in enumerate(choices):
    print index+1, item
# returns...
# Your choices are:
# 1 pizza
# 2 pasta
# 3 salad
# 4 nachos
# None 

#zip compares lists, stops at shorter
list_a = [3, 9, 17, 15, 19]
list_b = [2, 4, 8, 10, 30, 40, 50, 60, 70, 80, 90]

for a, b in zip(list_a, list_b):
    print max(a,b)    

def factorial(x):
    fact = 1
    for i in range(1, x+1):
        fact = fact * i
    return fact

