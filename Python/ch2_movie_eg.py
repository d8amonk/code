import pandas as pd

unames = ['user_id','gender','age','occupation','zip']
users = pd.read_table('c:/data_dump\ml-1m\users.dat', sep="::", header=None, names=unames)

rnames = ['user_id','movie_id','rating','timestamp']
ratings = pd.read_table("c:\data_dump\ml-1m\i_ratings.dat", sep="::", header=None, names=rnames)

mnames = ['movie_id','title','genres']
movies = pd.read_table("c:\data_dump\ml-1m\movies.dat", sep="::", header=None, names=mnames)

users[:5]
ratings[:5]
movies[:5]

#by (eg) age,sex would be easier with one table, so...(exploit overlapping names)
#first merge users with ratings (on user_id)
#second, merge the resulting table with movies on movie_id
merge_1 = pd.merge(users,ratings)
merge_2 = pd.merge(merge_1,movies)
#or, more simply
data = pd.merge(pd.merge(users,ratings),movies)
#proof
#sameness = merge_2==data
#sameness[:10]
#should all be true <- samesness.all() gives unanimity by cat
#[future use - gtg itertools.py]
# def unanimous(it):
#   it1, it2 = itertools.tee(it)
#   return all(it1) or not any(it2)

mean_ratings=data.pivot_table('rating', rows='title',cols='gender',aggfunc='mean')
#filter down to movies w >= 250 ratings
ratings_by_title = data.groupby('title').size()
active_titles = ratings_by_title.index[ratings_by_title>=250]
mean_ratings=mean_ratings.ix[active_titles]

top_female_ratings = mean_ratings.sort_index(by='F', ascending = False)
top_female_ratings[:10] #top ten movies by female ratings

#disagreement between genders
mean_ratings['diff'] = mean_ratings['M'] - mean_ratings['F']
sorted_by_diff = mean_ratings.sort_index(by='diff')
sorted_by_diff[:10]
#reverse rows, so now <- movies preferred by men
sorted_by_diff[::-1][:10]
#disagreement (ind. of gender) can be thought of as variance
rating_std_by_title = data.groupby('title')['rating'].std()
#active titles, descending order by rating
rating_std_by_title = rating_std_by_title.ix[active_titles].order(ascending=False) #multiple methods







