def reverse(text):
	n=len(text)-1
	R=""
	for i in range(n,-1,-1):
		R=R+text[i]
	return R
#len(cake) = 4 - 1 = 3 ... text[3]=e, [::-1], text[2] = k, [::-1], text[1] = a, [::-1], text[0] = c 
#append this sequence to empy list R, print R