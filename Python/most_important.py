randn(1000)

series = Series(randn(1000))
series
series.describe() #should be count = 1000

series[::2] = np.nan # the indexer method is for an index, ie works with series
series
series.describe() #should now be count = 500 bc 'every other' [::2] index was replaced with numpy's not-a-number placeholder NaN

#now try to apply the same [::] slice method to a dataframe's column

