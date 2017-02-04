import numpy as np

def heads_roll(trials, n_heads):
    counter = 0
    for _ in range(10000):
        heads = np.random.random(n_heads) < 0.5
        n = sum(heads)
        if n == n_heads:
            counter += 1
    print(counter/trials)
    
    