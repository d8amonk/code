import os
import pandas as pd
import numpy as np
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import chi2

os.chdir('d:/')
os.listdir('d:/')
xiv = pd.read_csv('XIV_jvg.csv')

xiv['Volume'].dtypes
xiv['Volume'].head(5)

xiv['Volume'] = xiv['Volume'].astype(float)
xiv['Volume'].dtypes
xiv.columns

X = np.array(xiv.ix[:,'Volume':'hilo']).astype(float)
y = np.array(xiv.ix[:,'Open']).astype(float)

X_new = SelectKBest(chi2, k = 2).fit_transform(X, y)

