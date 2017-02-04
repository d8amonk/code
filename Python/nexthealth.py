# let's use Quandl's API to ingest the data, as it's likely the quickest, 
# gets the most up-to-date data on each pull, and doesn't have to live in our system until we save
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd # popular DataFraming conventions
import Quandl


mydata = Quandl.get("NSE/OIL", authtoken="your token here")

mydata = Quandl.get("WIKI/AAPL")
mydata = Quandl.get("WIKI/AAPL", returns="numpy") # "pandas" and "numpy" formats
df2 = pd.DataFrame('ID' : pd.Series(1, index=list(range(100001)), dtype='float32'), 'Zip' : pd.Series(np.random.randint(10000, 99999, index=list(range(100001)),dtype='float32'), 'F' : 'foo' )

df2.dtypes