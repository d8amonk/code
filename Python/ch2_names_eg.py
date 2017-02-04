#CH02 movielens example
%cd c:/python27/pydata-book/ch02

import json
path = 'usagov_bitly_data2012-03-16-1331923249.txt'
open(path).readline()

#'list comprehension':
records = [json.loads(line) for line in open(path)]
records[0]
records[1]
print records[0]['tz']

#tz = time zone, u prepend = unicode
#count unique time zones rating movies
#NOTE: NOT every JSON entry has a tz, so first line won't work
time_zones = [rec['tz'] for rec in records]

time_zones = [rec['tz'] for rec in records if 'tz' in rec]

#counting using a dict to store counts
def get_counts(sequence):
		counts = {}
		for x in sequence:
			if x in counts:
				counts[x] += 1
			else:
				counts[x] = 1
		return counts

#alternate dictionary
# def get_counts2(sequence):
# 	counts = defaultdict(int) # values will initialize to 0
# 	for x in sequence:
# 		counts[x] += 1
# 	return counts

get_counts()

counts = get_counts(time_zones)
counts['America/New_York']
len(time_zones)

#top ten most frequently occurring time zones
def top_counts(count_dict, n=10):
	value_key_pairs = [(count,tz) for tz, count in count_dict.items()]
	value_key_pairs.sort()
	return value_key_pairs[-n:]

top_counts(counts)

#or call from Python library:
from collections import Counter
counts = Counter(time_zones)
counts.most_common(10)

#another approach to counting the tz
from pandas import DataFrame, Series
import pandas as pd; import numpy as np

frame = DataFrame(records)
frame['tz'][:10]

tz_counts = frame['tz'].value_counts()

#replace NA/unk with boolean array indexing
clean_tz = frame['tz'].fillna('Missing')
clean_tz[clean_tz ==''] = 'Unknown'
tz_counts = clean_tz.value_counts()
tz_counts[:10]

#barplot using plots method on counts object
tz_counts[:10].plot(kind='barh', rot=0)

#browser type
frame['a'][1]
frame['a'][50]
frame['a'][51]

#display plot by browser
results = Series([x.split()[0] for x in frame.a.dropna()])
results[:5]
results.value_counts()[:8]

#split again by OS, searching for OS.name anywhere in frame.a string
cframe = frame[frame.a.notnull()]
op_sys = np.where(cframe['a'].str.contains('Windows'), 'Windows', 'Not Windows')
by_tz_os = cframe.groupby(['tz', op_sys])

#group counts analogous to value_counts object above, uses size method; 
#then turned into a table with unstack method
agg_counts = by_tz_os.size().unstack().fillna()
agg_counts[:10]

#construct an indirect index array to selet top overall tz
#use to sort in ascending order
indexer = agg_counts.sum(1).argsort()
indexer[:10]

count_subset = agg_counts.take(indexer)[-10:]
count_subset

count_subset.plot(kind='barh',stacked=True)

#normalize the subset to make it easier to compare magnitudes
normed_subet = count_subset.div(count_subset.sum(1), axis=0)
normed_subet.plot(kind='barh',stacked=True)


#basic dict
d = {'a': 'apple', 'b': 'berry', 'c': 'cherry'}

for key in d:
    print key, d[key]

#menu selection
choices = ['pizza', 'pasta', 'salad', 'nachos']

print 'Your choices are:'
for index, item in enumerate(choices):
    print index+1, item
# returns...
# Your choices are:
# 1 pizza
# 2 pasta
# 3 salad
# 4 nachos
# None 

#zip compares lists, stops at shorter
list_a = [3, 9, 17, 15, 19]
list_b = [2, 4, 8, 10, 30, 40, 50, 60, 70, 80, 90]

for a, b in zip(list_a, list_b):
    print max(a,b)    