from random import randint

n = randint(1,101)
print n

def fizzbuzz(i):
    for n in range(1,i):
        if n % 5 == 0 and n % 3 == 0:
            print "fizzbuzz"
        elif n % 5 == 0:
            print "buzz"
        elif n % 3 == 0:
            print "fizz"   
        else:
            print n




