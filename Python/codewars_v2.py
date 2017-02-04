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


maxSequence([-2, 1, -3, 4, -1, 2, 1, -5, 4])
# should be 6: [4, -1, 2, 1]
arr = [-2, 1, -3, 4, -1, 2, 1, -5, 4]
if arr:
    print True

arr_test = []
if arr_test:
    print True


def maxSequence(arr):
    if arr:
        it = iter(arr)
        first = next(it)
        second = next(it)
        if first > second:
            for i in arr[1::]:
                f = first + i
                if f > first:
                    first += i
                    return first
                    print(str(first) + str(i))
                else:
                    return first
        else:
            for i in arr[2::]:
                s = second + i
                if s > second:
                    second += i
                    return second
                    print(str(second) + str(i))
                else:
                    return second
    else:
        return 0

import math
import numpy as np
a, b, c = 1,1,1
a*a + b*b == c*c

def triangle_type(a, b, c):
    # Should return triangle type:
    #  0 : if triangle cannot be made with given sides
    #  1 : acute triangle
    #  2 : right triangle
    #  3 : obtuse triangle

    if not ((c - b) < a < (c + b)) and ((a - c) < b < (a + c)) and ((b - a) < b < (b + a)):
        return 0
    elif a < b < c or b < a < c:
        # print a,b,c
        pass
    elif b < c < a or c < b < a:
        a, c = c, a
        # print a, b, c
    elif c < a < b or a < c < b:
        b, c = c, b
        # print a, b, c
    else:
        # print a, b, c
        pass

    gamma_1 = np.rad2deg(np.arccos((a * a + b * b - c * c) / (2.0 * a * b)))
    gamma_2 = np.rad2deg(np.arccos((c * c + a * a - b * b) / (2.0 * c * a)))
    gamma_3 = np.rad2deg(np.arccos((b * b + c * c - a * a) / (2.0 * b * c)))

    if (a * a + b * b == c * c) or (b * b + c * c == a * a) or (c * c + b * b == a * a):
        return 2
    elif max(gamma_1, gamma_2, gamma_3) < 90.0:
        return 1
    elif max(gamma_1, gamma_2, gamma_3) > 90.0:
        return 3
    else:
        return 0

triangle_type(7,3,2) # Not triangle 0
triangle_type(2,4,6) # Not triangle 0
triangle_type(8,5,7) # Acute 1
triangle_type(3,4,5) # Right 2
triangle_type(7,12,8) # Obtuse 3


# quadratic root sum
import math
def roots(a, b, c):

    d = b ** 2 - 4 * a * c  # discriminant

    if d < 0:
        return []
    elif d == 0:
        x = (-b + math.sqrt(d)) / (2 * a)
        return round(2*x, 2)
    else:
        x1 = (-b + math.sqrt(d)) / (2 * a)
        x2 = (-b - math.sqrt(d)) / (2 * a)
        x = x1 + x2
        return round(x, 2)

roots(1,-1,-20)
roots(2,8,0)
roots(1,-4,4) # careful!

# Hero's Root
def int_rac(n, guess, count=0):
    if abs(guess - (guess + n / guess) / 2) < 1:
        return count + 1
    else:
        return int_rac(n, (guess + n / guess) / 2, count + 1)


def int_rac(n, guess):
    cnt = 0

    while True:
        cnt += 1
        next_guess = (guess + n // guess) // 2

        if next_guess == guess:
            return cnt

        guess = next_guess


int_rac(25, 1)
int_rac(125348, 300)


l = ['a','b','c']

def shift(l, n):
    return l[n:] + l[:n]

def popfirst(l):
    l.reverse()
    a = l.pop()
    l.reverse()
    return a

def pop_shift(str):
    s1 = []
    s2 = []
    str = list(str)
    rem = list(str)

    def popfirst(l):
        l.reverse()
        a = l.pop()
        l.reverse()
        return a

    while len(rem) > 1:
        s1.append(str.pop())
        s2.append(popfirst(str))
        rem = str

    return [''.join(s1), ''.join(s2), ''.join(rem)]

str = "reusetestcasesbitcointakeovertheworldmaybewhoknowsperhaps"
pop_shift(str)


# who took the kahkeys
def who_took_the_car_key(message):
    dict = {'01000001': 'A',
            '01000010': 'B',
            '01000011': 'C',
            '01000100': 'D',
            '01000101': 'E',
            '01000110': 'F',
            '01000111': 'G',
            '01001000': 'H',
            '01001001': 'I',
            '01001010': 'J',
            '01001011': 'K',
            '01001100': 'L',
            '01001101': 'M',
            '01001110': 'N',
            '01001111': 'O',
            '01010000': 'P',
            '01010001': 'Q',
            '01010010': 'R',
            '01010011': 'S',
            '01010100': 'T',
            '01010101': 'U',
            '01010110': 'V',
            '01010111': 'W',
            '01011000': 'X',
            '01011001': 'Y',
            '01011010': 'Z',
            '01100001': 'a',
            '01100010': 'b',
            '01100011': 'c',
            '01100100': 'd',
            '01100101': 'e',
            '01100110': 'f',
            '01100111': 'g',
            '01101000': 'h',
            '01101001': 'i',
            '01101010': 'j',
            '01101011': 'k',
            '01101100': 'l',
            '01101101': 'm',
            '01101110': 'n',
            '01101111': 'o',
            '01110000': 'p',
            '01110001': 'q',
            '01110010': 'r',
            '01110011': 's',
            '01110100': 't',
            '01110101': 'u',
            '01110110': 'v',
            '01110111': 'w',
            '01111000': 'x',
            '01111001': 'y',
            '01111010': 'z'}

    m = []

    for char in message:
        m.append(dict[char])

    return("".join(m))

# arggggggh - BASE 2!!!!
def who_took_the_car_key(message):
    return ''.join(chr(int(i,2)) for i in message)
who_took_the_car_key(['01000001', '01101100', '01100101', '01111000', '01100001', '01101110', '01100100', '01100101', '01110010'])


# pin val
def validate_pin(pin):
    if len(pin) in (4,6) and all(p in '0123456789' for p in pin):
        return True
    else:
        return False

def validate_pin(pin):
    return len(pin) in (4, 6) and pin.isdigit()

validate_pin("1234")


def pig_it(text):
    s = ""

    for i in text.split():
        if i.isalnum():
            s += (i[1::] + str(i[0]) + 'ay ')
        else:
            s += (i[1::] + str(i[0]) + ' ')


    return(s[:-1])
pig_it('Pig Latin is cool') #igPay atinlay siay oolcay
pig_it('Pig Latin is cool !') #igPay atinlay siay oolcay?

def pig_it(text):
    lst = text.split()
    return ' '.join( [word[1:] + word[:1] + 'ay' if word.isalpha() else word for word in lst])