# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd


unames=['user_id', 'gender', 'age', 'occupation', 'zip']
users=pd.read_table('/Users/jvg/git/code/Python/py4ds/pydata-book/ch02/movielens/users.dat',
                      sep="::",
                      header=None,
                      names=unames,
                      engine='python')


rnames=['user_id', 'movie_id', 'rating', 'timestamp']
ratings=pd.read_table('/Users/jvg/git/code/Python/py4ds/pydata-book/ch02/movielens/ratings.dat',
                      sep="::",
                      header=None,
                      names=rnames,
                      engine='python')

mnames=['movie_id', 'title', 'genres']
movies=pd.read_table('/Users/jvg/git/code/Python/py4ds/pydata-book/ch02/movielens/movies.dat',
                      sep="::",
                      header=None,
                      names=mnames,
                      engine='python')

users[:5]
ratings[:5]
movies[:5]

data=pd.merge(pd.merge(ratings, users), movies)
data.iloc[0]

mean_ratings=pd.pivot_table(data=data,
                            values='rating',
                            index='title',
                            columns='gender',
                            aggfunc='mean')

ratings_by_title=data.groupby('title').size()

active_titles=ratings_by_title.index[ratings_by_title > 250]

active_mean_ratings = mean_ratings.ix[active_titles]
top_female_ratings = active_mean_ratings.sort_values(by='F', ascending=False)
top_female_ratings[:10]

mean_ratings['diff'] = active_mean_ratings['M'] - active_mean_ratings['F']
sorted_by_diff = mean_ratings.dropna().sort_values(by='diff')

sorted_by_diff[::-1][:15]

ratings_std_by_title = data.groupby('title')['rating'].std()
ratings_std_by_title = ratings_std_by_title.ix[active_titles]
ratings_std_by_title.sort_values(ascending=False)[:10]

