def is_prime(x):
    if x < 2:
        return False

	#for i in range(2, int(x ** 0.5) + 1) #this takes too long
    for i in range(2, int(x ** 0.5) + 1): #can't have a factor greater than its root
        if x % i == 0:
            return False

    return True