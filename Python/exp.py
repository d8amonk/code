import numpy as np
import pandas as pd

randn = np.random.randn

s1 = Series(randn(5), index['a','b','c','d','e'])
s1

s2 = Series(randn(10))
s2

s3 = Series(randn(100))
s3

s4 = Series(2*randn(100))
s4

s5 = Series(10*randn(100))
s5

np.exp(s1)
np.exp(s2)

scatter(s1,np.exp(s1))
scatter(s2,np.exp(s2))
scatter(s3,np.exp(s3))
scatter(s4,np.exp(s4))
scatter(s5,np.exp(s5))

