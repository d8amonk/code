from pandas import *
import pandas as pd
#make sure you're in pylab or import numpy

#%cd "C:\Users\Jeffrey\Google Drive\Code"
police = pd.read_csv("C:\Users\Jeffrey\Google Drive\Code\NPMRP_Data_2009_2010.csv")

police.columns = ['locale','year','type','status','detail','URL']
police['state'] = police['locale'].str.split(', ').str.get(1)
