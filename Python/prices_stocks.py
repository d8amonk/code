prices = {
    "banana" : 4,
    "apple" : 2,
    "orange" : 1.5,
    "pear" : 3
}

stock = {
    "banana" : 6,
    "apple" : 0,
    "orange" : 32,
    "pear" : 15
}

for i in prices:
    print i
    print "price: %s" % prices[i]
    print "stock: %s" % stock[i]

#sell it all for...?
total = 0
for i in prices: 
    rev = prices[i] * stock[i]
    total = total + rev

print total

#go shopping
def compute_bill(food):
    total = 0
    for i in food:
        if stock[i] > 0:
            stock[i] = stock[i] - 1
            total += prices[i]
    return total