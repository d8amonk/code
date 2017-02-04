import pandas as pd

names1880 = pd.read_csv('c:\data_dump\inames\yob1880.txt', names=['name','sex','births'])
names1880.groupby('sex').births.sum()

#data is from 1880 - 2010
years = range(1880,2010)
pieces = []
columns = ['name','sex','births']

#assemble the yearly birth data ...
for year in years:
	path = 'yob%d.txt' % year
	frame = pd.read_csv(path, names=columns)

	frame['year'] = year
	pieces.append(frame)
#...and concat all into a unified dataframe 
names = pd.concat(pieces, ignore_index=True)

total_births = names.pivot_table('births',rows='year', cols='sex', aggfunc=sum)
total_births.tail()

total_births.plot(title="Total births by sex and year")

#birth name proportion, by year
def add_prop(group):
	#Integer division floors
	births = group.births.astype(float)

	group ['prop'] = births/births.sum()
	return group
names = names.groupby(['year','sex']).apply(add_prop)

#sanity check props~1
import numpy as np
np.allclose(names.groupby(['year','sex']).prop.sum(),1) #Should return 1=true

#top 1000 names for each sex/year combo
def get_top1000(group):
	return group.sort_index(by='births', ascending=False)[:1000]

grouped = names.groupby(['year','sex'])
top1000 = grouped.apply(get_top1000)

#simple time series (munging first)
boys = top1000[top1000.sex=='M']
girls = top1000[top1000.sex=='F']

total_births = top1000.pivot_table('births',rows='year',cols='name', aggfunc = sum)
total_births #LOOK AT THE DIM, ZULA!

subset = total_births[['John','Harry','Mary','Marilyn']]
subset.plot(subplots = True, figsize =(12,10), grid = False, title = "Number of births per year")

#measuring naming diversity (names above didn't become 'unpopular', the field of names increased)
table = top1000.pivot_table('prop',rows='year',cols='sex', aggfunc=sum)
table.plot(title = 'Sum of top1000.prop by year and sex', yticks=np.linspace(0,1.2,13), xticks=range(1880,2020,10))

#Number of distinct names in the top 50%
df = boys[boys.year == 2010]
df



