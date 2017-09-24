# -*- coding: utf-8 -*-
import numpy as np
import pandas as pd


unames = ['user_id', 'gender', 'age', 'occupation', 'zip']
users = pd.read_table("/Users/jvg/git/code/Python/py4ds/pydata-book/ch02/movielens/users.dat",
                      sep = "::",
                      header = None,
                      names = unames)


rnames = ['user_id', 'movie_id', 'rating', 'timestamp']
ratings = pd.read_table("/Users/jvg/git/code/Python/py4ds/pydata-book/ch02/movielens/ratings.dat",
                      sep = "::",
                      header = None,
                      names = rnames)

mnames = ['movie_id', 'title', 'genres']
movies = pd.read_table("/Users/jvg/git/code/Python/py4ds/pydata-book/ch02/movielens/movies.dat",
                      sep = "::",
                      header = None,
                      names = unames)

