def count_small(numbers): #numbers is a list or dict obj; covariates?
    total = 0
    for n in numbers: 
        if n < 10:
            total = total + 1
    return total

lost = [4, 8, 15, 16, 23, 42]
small = count_small(lost)
print small
    
    #dropped_obs = 0 
    #for i in n
    #	if abs(i) > abs(t=1.96): #one thing that would be nice : universal option for critical values?
    #		del i #or just count as 'outlier'?
    #     	dropped = dropped + 1
