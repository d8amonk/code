def is_even(x):
    if x % 2 == 0:
        return True
    else:
        return False

def is_int(x):
    if x-int(x)==0:
        return True
    else:
        return False

def digit_sum(n):
    total = 0
    for i in str(n):
        total += int(i)
    return total  

def factorial(x):
    fact = 1
for i in range(1, x+1):
        fact = fact * i
    return fact

def is_prime(x):
    if x < 2:
        return False

    for i in range(2, x):
        if x % i == 0:
            return False

    return True

def reverse(text):
	n=len(text)-1
	R=""
	for i in range(n,-1,-1):
		R=R+text[i]
	return R

def anti_vowel(text):
    v = ""
    for c in text:
        if c not in 'aeiouAEIOU':
            v += c
    return v

##scrabble
score = {"a": 1, "c": 3, "b": 3, "e": 1, "d": 2, "g": 2, 
         "f": 4, "i": 1, "h": 4, "k": 5, "j": 8, "m": 3, 
         "l": 1, "o": 1, "n": 1, "q": 10, "p": 3, "s": 1, 
         "r": 1, "u": 1, "t": 1, "w": 4, "v": 4, "y": 4, 
         "x": 8, "z": 10}
         
def scrabble_score(word):
    total = 0

    for i in word.lower():
        total += score[i]
    return total 

def censor(text,word):
    return text.replace(word, "*" * len(word))
    
censor("this hack is wack hack", "hack") 


def count(sequence,item):
    found = list()
    for i in range(len(sequence)):
        if sequence[i] == item:
            found.append(item)
    return len(found)

def purify(seq):
    op = []
    for i in seq:
        if i % 2 == 0:
            op.append(i)
    return op

def product(num_list):
    total = 0
    for num in num_list:
        if total == 0:
            total =+ num
        else:
            total *= num
    return total

def remove_duplicates(lst):
    x = []
    for e in lst:
        if e not in x:
            x.append(e)
    return x
 #same as
 def remove_duplicates(f):
    return list(set(f))

def median(lst): #eg[4,5,5,4]
    s = sorted(lst) #[4,4,5,5]
    x = len(s) #4
    if x%2 == 0: #4%2==0
        y = x/2 #2
        z = y-1 #1
        w = (s[y]+s[z])/2.0 #float div
        return w
    elif x == 1:
        return s[0]
    else:
        return s[x/2] #int div

#"EXAM STATS"
grades = [100, 100, 90, 40, 80, 100, 85, 70, 90, 65, 90, 85, 50.5]
print(grades)

def print_grades(grades):
    for grade in grades:
        print grade
print_grades(grades) #notice no print() call -- it's in the f()

def grades_sum(grades):
    total = 0
    for grade in grades: 
        total += grade
    return total
print grades_sum(grades)

def grades_average(grades):
    sum_of_grades = grades_sum(grades)
    average = sum_of_grades / float(len(grades))
    return average
print grades_average(grades)

def grades_variance(scores):
    average = grades_average(scores)
    variance = 0
    
    for score in scores:
        variance += (average - score)**2
    return variance/len(scores)
print grades_variance(grades)

def grades_std_deviation(variance):
    return variance **0.5
variance = grades_variance(grades)
print grades_std_deviation(variance)#END "EXAM STATS"

#list comprehension
evens_to_50 = [i for i in range(51) if i % 2 == 0] 
print evens_to_50

doubles_by_3 = [x*2 for x in range(1,6) if (x*2) % 3 == 0]
print doubles_by_3

even_squares = [x**2 for x in range(1,12) if x%2==0]
print even_squares

cubes_by_four = [x**3 for x in range(1,11) if (x**3)%4==0]
print c

l = [i ** 2 for i in range(1, 11)]
#[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
print l[2:9:2]
#[9, 25, 49, 81]

#reverse a list; remember: [start:stop:stride]
my_list = range(1, 11)
backwards = my_list[::-1]

to_one_hundred = range(101)
backwards_by_tens = to_one_hundred[::-10]
print backwards_by_tens

#anonymous f with lambda
languages = ["HTML", "JavaScript", "Python", "Ruby"]
print filter(lambda x: x == 'Python', languages)

squares = [x**2 for x in range(1,11)]
print filter(lambda x: x>=30 and x<=70, squares) #squares between 30, 70 incl.

threes_and_fives = [x for x in range(1,16) if x%3==0 or x%5==0]

#SECRET MESSAGE
garbled = "!XeXgXaXsXsXeXmX XtXeXrXcXeXsX XeXhXtX XmXaX XI"
message = garbled[::-1][::2]
print message

garbled = "IXXX aXXmX aXXXnXoXXXXXtXhXeXXXXrX sXXXXeXcXXXrXeXt mXXeXsXXXsXaXXXXXXgXeX!XX"
message = filter(lambda x: x != 'X',garbled)
print message

#bitwise / base 2 - note powers of 2: 
#10, 100, 1000, 10000 
#(base 10 digits: powers of 10)
one = 0b1
two = 0b10 #2
three = 0b11
four = 0b100 #4
five = 0b101
six = 0b110
seven = 0b111 
eight = 0b1000 #8
nine = 0b1001 
ten = 0b1010 
eleven = 0b1011 
twelve = 0b1100
thirteen = 0b1101
fourteen = 0b1110
fifteen = 0b1111
sixten = 0b10000 #16

for i in range(2,6):
    print bin(i)

int("1110100101001",2)
#Out: 7465

# Print out the decimal equivalent of the binary 11001001.
print int("11001001",2) #201
print int("1",2) #1
print int("10",2) #2
print int("111",2) #7
print int("0b100",2) #4
print int(bin(5),2) #5

#bit shifting
shift_right = 0b1100 >> 2
shift_left = 0b1 << 2
print shift_right
print shift_left
print bin(shift_right)
print bin(shift_left)

#bitwise AND (&) can only give results <= a&b
#bitwise OR (|) can only give results >= a&b
#XOR-ing ~ a number with itself (a^a) will always result in 0
##this looks like 'add one and make it negative'
## ~/XOR: either dig[i] is 1, but NOT BOTH ('exclusive or')
print ~1 # -2
print ~2 # -3
print ~3 # -4
print ~42 # -43
print ~123 # -124
a = 0b110 # 6
mask = 0b111 # 7
desired =  a ^ mask # 0b1
a = 0b11101110 #238
mask = 0b11111111 #255 (eg: 8-bit color)
print bin(a^mask) #0b10001 = 17 (DIFFERENCE)

# bitmasking 
num  = 0b1100
mask = 0b0100
desired = num & mask
if desired > 0:
    print "Bit was on"

# bitmasking eg: checking digits
def check_bit4(input):
    check = 0b1000 
    desired = input & check
    if desired > 0:
        return "on"
    else:
        return "off"

# turn a bit on - only works with even numbers (1's bit = 0):
a = 0b110 # 6
mask = 0b1 # 1
desired =  a | mask # 0b111, or 7
#alt
a = 0b100110
check = 0b1
def addone(num):
    out = num | check
    print out

#bitshifting and masking:
a = 0b101 
# Tenth bit mask / turn on 10th bit from right
mask = (0b1 << 9)  # One less than ten 
desired = a ^ mask
print desired

def flip_bit(number, n):
    mask = (0b1<<n-1)
    result = number ^ mask
    return bin(result)

#CLASSES
class Fruit(object):
    # A class that makes various tasty fruits.
    def __init__(self, name, color, flavor, poisonous):
        self.name = name
        self.color = color
        self.flavor = flavor
        self.poisonous = poisonous

    def tellme(self):
        print "I'm a %s %s and I taste %s." % (self.color, self.name, self.flavor)

    def is_edible(self):
        if not self.poisonous:
            print "Yep! I'm edible."
        else:
            print "Don't eat me! I am super poisonous."
lemon = Fruit("lemon", "yellow", "sour", False)
lemon.description()
lemon.is_edible()

class Animal(object):
    def __init__(self, name):
        self.name = name
zebra = Animal("Jeffrey")
print zebra.name

class Animal(object):
    """Makes cute animals."""
    is_alive = True
    def __init__(self, name, age):
        self.name = name
        self.age = age
    def description(self):
        print self.name
        print self.age
hippo = Animal("Henry", 12)
hippo.description

#ShoppingCart
class ShoppingCart(object):
    """Creates shopping cart objects
    for users of our fine website."""
    items_in_cart = {}
    def __init__(self, customer_name):
        self.customer_name = customer_name

    def add_item(self, product, price):
        """Add product to the cart."""
        if not product in self.items_in_cart:
            self.items_in_cart[product] = price
            print product + " added."
        else:
            print product + " is already in the cart."

    def remove_item(self, product):
        """Remove product from the cart."""
        if product in self.items_in_cart:
            del self.items_in_cart[product]
            print product + " removed."
        else:
            print product + " is not in the cart."
my_cart = ShoppingCart('jeff') 
# __init__ takes one argument, 
# but add_items takes two
my_cart.add_item("apple", 3)
#########END ShoppingCart

#customer
class Customer(object):
    """Produces objects that represent customers."""
    def __init__(self, customer_id):
        self.customer_id = customer_id

    def display_cart(self):
        print "I'm a string that stands in for the contents of your shopping cart!"

class ReturningCustomer(Customer):
    """For customers of the repeat variety."""
    def display_order_history(self):
        print "I'm a string that stands in for your order history!"

monty_python = ReturningCustomer("ID: 12345")
monty_python.display_cart() #inheritance of display_cart method
monty_python.display_order_history()
########END customer

class Employee(object):
    def __init__(self, name):
        self.name = name
    def greet(self, other):
        print "Hello, %s" % other.name

class CEO(Employee):
    def greet(self, other): #overrides the base Employee.greet method
        print "Get back to work, %s!" % other.name
ceo = CEO("Emily")
emp = Employee("Steve")
emp.greet(ceo)
# Hello, Emily
ceo.greet(emp)
# Get back to work, Steve!

#Triangle inheritance 
class Triangle(object):
    number_of_sides = 3
    def __init__(self, angle1, angle2, angle3):
        self.angle1 = angle1
        self.angle2 = angle2
        self.angle3 = angle3
    def check_angles(self):
        if self.angle1 + self.angle2 + self.angle3 == 180:
            return True 
        else:
            return False
                
my_triangle = Triangle(90,30,60)

print my_triangle.number_of_sides
print my_triangle.check_angles()

class Equilateral(Triangle):
    angle = 60
    def __init__(self):
        self.angle1 = self.angle2 = self.angle3 = self.angle
##END Triangle

#Where we're going, we don't need roads
class Car(object):
    condition = "new"
    def __init__(self, model, color, mpg):
        self.model = model
        self.color = color
        self.mpg   = mpg
    def display_car(self):
        print "This is a %s %s with %d MPG." % (self.color, self.model, self.mpg)
    def drive_car(self):
        self.condition = "used"

#my_car = Car("DeLorean", "silver", 88)
#print my_car.condition
#my_car.drive_car()
#print my_car.condition

class ElectricCar(Car):
    def __init__(self, model, color, mpg, battery_type):
        super(ElectricCar, self).__init__(model, color, mpg)
        self.battery_type = battery_type
    def drive_car(self):
        self.condition = "like new"

my_car = ElectricCar("Jeffs", "red and white", 112, "molten salt")

print my_car.condition
my_car.drive_car()
print my_car.condition

#point in R3
class Point3D(object):
    def __init__(self,x,y,z):
        self.x = x
        self.y = y
        self.z = z
    def __repr__(self):
        return "(%s, %s, %s)" % (str(self.x), str(self.y),str(self.z))

my_point = Point3D(1,2,3)
print my_point

#fileIO
my_list = [i**2 for i in range(1,11)]
# Generates a list of squares of the numbers 1 - 10

f = open("output.txt", "w")

for item in my_list:
    f.write(str(item) + "\n")
f.close()

my_file = open("output.txt","r")
print my_file.read()
my_file.close()

#implicit close
with open("text.txt", "w") as textfile:
    textfile.write("Success!")

