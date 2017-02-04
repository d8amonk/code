#Gallup 'atheism and religiosity 2012' survey charted yes response to atheism by country 
#in 2005 and 2012
us12 = subset(atheism, atheism$nationality == "United States" & atheism$year == 
    "2012")

# Calculate the proportion of atheist responses in US (should be 5%):
proportion = nrow(subset(us12, response == "atheist"))/nrow(us12)