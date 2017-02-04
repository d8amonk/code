# fb likes
def likes(names):
    if len(names) == 0:
        print("no one likes this")
    if len(names) == 1:
        print(names[0] + " likes this")
    elif len(names) == 2:
        print(names[0] + " and " + names[1] + " like this")
    elif len(names) == 3:
        print(names[0] + ", " + names[1] + " and " + names[2] + " like this")
    elif len(names) >= 4:
        print(str(names[0]) + ", " + str(names[1]) + " and " + str(len(names) - 2) + " others like this")

likes([])
likes(['Peter'])
likes(['Jacob', 'Alex'])
likes(['Max', 'John', 'Mark'])
likes(['Alex', 'Jacob', 'Mark', 'Max'])
likes(['Alex', 'Jacob', 'Mark', 'Max', 'Luke'])

# fred eg
x = [1,2,3,4,5]

def en(thing):
    for index, i in enumerate(thing):
        print index, i

start = 1
end = 5
step = 2

x[start:end:step]
'race car'[::-1].split(' ')[::-1]
print list.split(' ').reverse()

# morse code

def morse_machine():

    def msg():
        global msg_type
        msg_type = raw_input('"SENDING" OR "RECEIVING" CODE?: ').upper()
        global msg_in
        msg_in = raw_input('MESSAGE: ').upper()
    msg()

    a2msg = {'A': '.-', 'B': '-...', 'C': '-.-.',
            'D': '-..', 'E': '.', 'F': '..-.',
            'G': '--.', 'H': '....', 'I': '..',
            'J': '.---', 'K': '-.-', 'L': '.-..',
            'M': '--', 'N': '-.', 'O': '---',
            'P': '.--.', 'Q': '--.-', 'R': '.-.',
            'S': '...', 'T': '-', 'U': '..-',
            'V': '...-', 'W': '.--', 'X': '-..-',
            'Y': '-.--', 'Z': '--..',

            '0': '-----', '1': '.----', '2': '..---',
            '3': '...--', '4': '....-', '5': '.....',
            '6': '-....', '7': '--...', '8': '---..',
            '9': '----.',

            '0': '-----', '1': '.----', '2': '..---',
            '3': '...--', '4': '....-', '5': '.....',
            '6': '-....', '7': '--...', '8': '---..',
            '9': '----.',

            ' ': '/', '.': '.-.-.-', ',': '--..--',
            ':': '---...', '?': '..--..', "'": '.----.',
            '-': '-....-', '/': '-..-.', '@': '.--.-.',
            '=': '-...-', '(': '-.--.', ')': '-.--.-',
            '+': '.-.-.'
            }

    msg2a = {v:k for k, v in a2msg.items()}

    if msg_type == 'SENDING':
        for char in msg_in:
            print a2msg[msg_in]

    elif msg_type == 'RECEIVING':
        for char in msg_in:
            print msg2a[msg_in]
    else:
        print "Don't know if you're sending (encoding) or receiving (unencoding)... :( ..."


# sum of digits
def digital_root(n):
    sum = 0
    while n:
        sum, n = sum + n % 10, n // 10
    return sum

def digital_root(n):
    x = sum(int(digit) for digit in str(n))
    if x < 10:
        return x
    else:
        return digital_root(x)
# import test
digital_root(12312656816161461464614641649648419614961365)
%timeit digital_root(12312656816161461464614641649648419614961365)

# remove the minimum
numbers = [1,22,2,1,22,11,2]
def remove_smallest(numbers):
    if len(numbers) == 0:
        []
    else:
        numbers.remove(min(numbers))
        numbers
remove_smallest(numbers)

# won't work - want the remaining values in the original order
def remove_smallest(numbers):
    numbers.sort()
    return numbers[1:]

# bp
def remove_smallest(numbers):
    if numbers:
        numbers.remove(min(numbers))
    return numbers

# gauss
def f(n):
    if n == 0:
        return 1
    else:
        x = 0
        for i in range(1,n+1):
            x += str(i)
        return x, type(x)
f(7)

# oddsnends
def find_outlier(integers):
    if [x for x in integers if x % 2 == 0]:
       return [x for x in integers if x % 2 == 0]
    elif [x for x in integers if x % 2 != 0]:
       return [x for x in integers if x % 2 != 0]
    else:
        print "wtf!"

def find_outlier(integers):
    o = []
    e = []
    for i in integers:
        if i % 2 == 0:
            e.append(i)
        else:
            o.append(i)

    if len(o) == 1:
        return sum(o)
    else:
        return sum(e)

o = [1,3,4,5]
e = [2,4,6,7]

find_outlier(o)
find_outlier(e)

# laprunner
def gcd(a, b):
    """Return greatest common divisor using Euclid's Algorithm."""
    while b:
        a, b = b, a % b
        print a, b
    return a

def lcm(a, b):
    """Return lowest common multiple."""
    return a * b // gcd(a, b)

def lcmm(*args):
    """Return lcm of args."""
    return reduce(lcm, args)

def nbr_of_laps(x,y):
    pass

# ???array_diff
a,b = [1,2,2,2,3],[2]


def array_diff(a, b):
    return set(a)-set(b)

# max sum of given digits
arr 