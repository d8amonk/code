import numpy as np

str_1 = 'xyxyxy'
str_2 = '110100'
l = [[str_1, str_2], [str_2, str_1]]
def populate(str_1, str_2, n = 100):
    l = []
    def crossover(str_1 = str_1, str_2 = str_2, n = n):
        for i in range(1, n/10):
            if np.random.randint(0,2):
                cut = np.random.randint(2, len(min(str_1, str_2)))
                new_str_1 = str_1[:cut] + str_2[cut:]
                new_str_2 = str_2[:cut] + str_1[cut:]
                l.append([new_str_1, new_str_2])
                i += 1
            else:
                l.append([str_1, str_2])
                i += 1
    def mutate_m(l = l, m = 1):
        for i in range(0, m):
            pair = l[np.random.randint(0, len(l))]
            pick = pair[np.random.randint(0, len(pair))]
            gene = pick[np.random.randint(0, len(pick))]
            first_bits = l[pair][pick][:gene]
            second_bits = l[pair][pick][(gene+1):]
            new_member = first_bits + 'Z' + second_bits

    crossover()
    mutate_m()
    return l
    return new_member

populate(str_1, str_2, n = 100)


    interim_pop = []
    for j in range(0, 10):
        draw = np.random.shuffle(l)[1]




    return(l)