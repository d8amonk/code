# The 'mlb11' data frame is already loaded into the workspace
load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/mlb11.RData"))

# Use the 'plot_ss' function to draw an estimated regression line. Two
# points are given to get you started:
x1 = 5400
y1 = 550

x2 = 5700
y2 = 850

plot_ss(x = mlb11$at_bats, y = mlb11$runs, x1, y1, x2, y2) 
#try adding 'leastSquares = T' and ''''''showSquares=T''''''

lm(formula = runs ~ at_bats, data = mlb11)

#Coefficients:
#(Intercept)      at_bats  
# -2789.2429       0.6305  
summary(lm(runs ~ homeruns, data=mlb11))

# The linear model:
m1 = lm(runs ~ at_bats, data = mlb11)

# Plot the least squares line:
plot(mlb11$at_bats, mlb11$runs)
abline(m1)