#Here are the vectors of variations or tweaks
sltweak <- c(.01, .02, .03, -.01, -.02, -.03) #one for the slope
ictweak <- c(.1, .2, .3, -.1, -.2, -.3)  #one for the intercept
lhs <- numeric()
rhs <- numeric()
#left side of eqn is the sum of squares of residuals of the tweaked regression line
for (n in 1:6) lhs[n] <- sqe(ols.slope+sltweak[n],ols.ic+ictweak[n])
#right side of eqn is the sum of squares of original residuals + sum of squares of two tweaks
for (n in 1:6) rhs[n] <- sqe(ols.slope,ols.ic) + sum(est(sltweak[n],ictweak[n])^2)

lhs - rhs #should be damn close to zero

#alternatively,
all.equal(lhs,rhs)

#calculate the variances in child height, the residuals, and the estimates
varChild <- var(galton$child)
varRes <- var(fit$residuals)
varEst <- var(est(ols.slope,ols.ic))

#compare the variances
all.equal(varChild,varRes+varEst)

efit <- lm(accel ~ mag + dist, attenu)
mean(efit$residuals) #alternatively all.equal(mean(efit$residuals),0)

#compare the variances of the magnitude predictor and the residuals
cov(efit$residuals, attenu$mag)
cov(efit$residuals, attenu$dist)

sqrt(sum(fit$residuals^2/(n-2)))
#same as
summary(fit)$sigma
#same as 
sqrt(deviance(fit)/(n-2))

#plot the original Galton data points with larger dots for more freq pts
y <- galton$child
x <- galton$parent
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)), 
     pch = 21, col = "black", bg = "lightblue",
     cex = .07 * freqData$freq, xlab = "parent", ylab = "child")

#original regression line, children as outcome, parents as predictor
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x) * cor(y, x),  #slope
       lwd = 3, col = "red")

#new regression line, parents as outcome, children as predictor
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
       sd(y) / cor(y, x) / sd(x), #slope
       lwd = 3, col = "blue")

#assume correlation is 1 so slope is ratio of std deviations
abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x),  #slope
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection

#Total Variation = Residual Variation + Regression Variation
#Bulding the R^2 estimates
mu <- mean(galton$child)
#TSS
sTot <- sum((galton$child-mu)^2)
#RSS
sRes <- deviance(fit)
#R^2 = ESS/TSS =
1 - sRes/sTot
#ESS = (1-sRes/sTot)*sTot
#R^2 = ((1-sRes/sTot)*sTot)/sTot =
summary(fit)$r.squared # =
cor(galton$child,galton$parent)^2

#lets demonstrate the purpose of the intercept, a special regressor of '1s'
ones <- rep(1,nrow(galton))
#compare lm(child ~ parent, galton)
lm(child ~ ones + parent -1, galton)

# The mean of a variable is the coefficient of its regression against the constant, 1. Thus, subtracting the mean is
# equivalent to replacing a variable by the residual of its regression against 1. In an R formula, the constant regressor
# can be represented by a 1 on the right hand side. Thus, the expression, lm(child ~ 1, galton), regresses child against
# the constant, 1. Recall that in the galton data, the mean height of a child was 68.09 inches. Use lm(child ~ 1, galton)
# to compare the resulting coefficient (the intercept) and the mean height of 68.09. Since we want the result to print,
# don't assign it a name.

# Regress the given variable on the given predictor,
# suppressing the intercept, and return the residual.
regressOneOnOne <- function(predictor, other, dataframe){
  # Point A. Create a formula such as Girth ~ Height -1
  formula <- paste0(other, " ~ ", predictor, " - 1")
  # Use the formula in a regression and return the residual.
  resid(lm(formula, dataframe))
}

# Eliminate the specified predictor from the dataframe by
# regressing all other variables on that predictor
# and returning a data frame containing the residuals
# of those regressions.
eliminate <- function(predictor, dataframe){
  # Find the names of all columns except the predictor.
  others <- setdiff(names(dataframe), predictor)
  # Calculate the residuals of each when regressed against the given predictor
  temp <- sapply(others, function(other)regressOneOnOne(predictor, other, dataframe))
  # sapply returns a matrix of residuals; convert to a data frame and return.
  as.data.frame(temp)
}

# The general technique is to pick one predictor and to replace all other variables by the residuals of their regressions
# against that one. The function, regressOneOnOne, in eliminate.R performs the first step of this process. Given the name
# of a predictor and one other variable, other, it returns the residual of other when regressed against predictor. In its
# first line, labeled Point A, it creates a formula.

# Regress the given variable on the given predictor,
# suppressing the intercept, and return the residual.
regressOneOnOne <- function(predictor, other, dataframe){
  # Point A. Create a formula such as Girth ~ Height -1
  formula <- paste0(other, " ~ ", predictor, " - 1")
  # Use the formula in a regression and return the residual.
  resid(lm(formula, dataframe))
}

# eliminate, applies regressOneOnOne to all variables except a given predictor and collects the
# residuals in a data frame. We'll first show that when we eliminate one regressor from the data, a regression on the
# remaining will produce their correct coefficients. (Of course, the coefficient of the eliminated regressor will be
# missing

# Eliminate the specified predictor from the dataframe by
# regressing all other variables on that predictor
# and returning a data frame containing the residuals
# of those regressions.
eliminate <- function(predictor, dataframe){
  # Find the names of all columns except the predictor.
  others <- setdiff(names(dataframe), predictor)
  # Calculate the residuals of each when regressed against the given predictor
  temp <- sapply(others, function(other)regressOneOnOne(predictor, other, dataframe))
  # sapply returns a matrix of residuals; convert to a data frame and return.
  as.data.frame(temp)
}

#R dataset, Swiss demo info, 1888
makelms <- function(){
  # Store the coefficient of linear models with different independent variables
  cf <- c(coef(lm(Fertility ~ Agriculture, swiss))[2], 
          coef(lm(Fertility ~ Agriculture + Catholic,swiss))[2],
          coef(lm(Fertility ~ Agriculture + Catholic + Education,swiss))[2],
          coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination,swiss))[2],
          coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination +Infant.Mortality, swiss))[2])
  print(cf)
}
makelms()
# Agriculture Agriculture Agriculture Agriculture Agriculture 
#   0.1942017   0.1095281  -0.2030377  -0.2206455  -0.1721140 

#R dataset, n =72 insect sprays in 12 x 6 groups.
fit <- lm(count ~ spray, InsectSprays)
est <- summary(fit)$coef[,1] 
est #14.5, +.83, etc.
#these coefs are estimated relative to spray A. est w/o cons to find explicit estimates
nfit <- lm(count ~ spray -1, InsectSprays)
summary(nfit)$coef #14.5, 15.33, etc.

#relevel F() changes the reference group
spray2 <- relevel(InsectSprays$spray,"C")
fit2 <- lm(count~spray2,InsectSprays)
summary(fit2)$coef #Spray A still = 14.5, etc, just diff ref grp.
mean(sC) #calculate spray C's mean (sC)
#calculate the t-value that appears in the summary table: 
(fit$coef[2] - fit$coef[3])/1.6011

#hatvalues() calculates inlfuence under the following setting:
fit <- lm(y ~ x, out2)
plot(fit, which = 1)
#fit includes the o/l, fitno doesn't
fitno <- lm(y ~ x, out2[-1,])
plot(fitno, which = 1) 
#which=3 uses a scale-location display 
#which=2 uses a Q-Q (standardized to theoretical norm.)
coef(fit)- coef(fitno) #or just use dfbeta()
head(dfbeta(fit))
resno <- out2[1,"y"] - predict(fitno, out2[1,]) #think machine learning here
1-resid(fit)[1]/resno #ratio of residual with/(div)/without the potential OL
#head(hatvalues(fit))

#Studentized residuals 
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno)) #using the sample standard deviation ...
resid(fit)[1]/(sigma1*sqrt(1-hatvalues(fit)[1])) #studentized resids, alternatively...
head(rstudent(fit)) #the rstudent() function

#Cook's distance is essentially the sum of squared differences between
#values fitted with and without a particular sample;
#tells how much a given sample changes a model
#(remember, "normalized" just means "divided by, in order that it can fit something else")

#so we're going to figure out how much an outlier affects the fitted values
#fit includes the o/l, fitno doesn't
sigma <- sqrt(deviance(fit)/df.residual(fit)) #sample SD of resid
dy <- predict(fitno, out2) - predict(fit,out2) #diff in FVs not/incl. outlier
sum(dy^2)/(2*sigma^2) #Cook's distance, alternatively...
cooks.distance(fit)[1] #or,
plot(fit, which = 5) #REALLY intuitive graph

#SIMULATION
# The function rgp1() computes the variance in estimates of the coefficient of x1 in each of the three
# models, y ~ x1, y ~ x1 + x2, and y ~ x1 + x2 + x3. (The results are rounded to 5 decimal places for
# convenient viewing.) This simulation approximates the variance (i.e., squared standard error) of x1's
# coefficient in each of these three models. Recall that variance inflation is due to correlated regressors
# and that in rgp1() the regressors are uncorrelated.

makelms <- function(x1, x2, x3){
  # Simulate a dependent variable, y, as x1
  # plus a normally distributed error of mean 0 and 
  # standard deviation .3.
  y <- x1 + rnorm(length(x1), sd = .3)
  # Find the coefficient of x1 in 3 nested linear
  # models, the first including only the predictor x1,
  # the second x1 and x2, the third x1, x2, and x3.
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
}

# Regressor generation process 1.
rgp1 <- function(){
  print("Processing. Please wait.")
  # number of samples per simulation
  n <- 100
  # number of simulations
  nosim <- 1000
  # set seed for reproducability
  set.seed(4321)
  # Point A
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  # Point B
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}

# Regressor generation process 2.
rgp2 <- function(){
  print("Processing. Please wait.")
  # number of samples per simulation
  n <- 100
  # number of simulations
  nosim <- 1000
  # set seed for reproducability
  set.seed(4321)
  # Point C
  x1 <- rnorm(n)
  x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
  x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
  # Point D
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}

rgp1()
rgp2()
#in rgp2, x2 and x3 are cor w x1 (cor(x3,x1)>cor(x2,x1))...

# A variance inflation factor (VIF) is a ratio of estimated variances, the variance due to including the ith
# regressor, divided by that due to including a corresponding ideal regressor which is uncorrelated with the
# others. VIF's can be calculated directly, but the car package provides a convenient method for the purpose
# as we will illustrate using the Swiss data from the datasets package.

mdl <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, swiss)

vif(mdl)
#      Agriculture      Examination        Education         Catholic Infant.Mortality 
#         2.284129         3.675420         2.774943         1.937160         1.107542 

# These VIF's show, for each regression coefficient, the variance inflation due to including all the others.
# For instance, the variance in the estimated coefficient of Education is 2.774943 times what it might have
# been if Education were not correlated with the other regressors. Since Education and score on an
# Examination are likely to be correlated, we might guess that most of the variance inflation for Education
# is due to including Examination. VIF is the square of standard error inflation. Compare mdl to:

mdl2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, swiss)
vif(mdl2) #Ed goes to 1.82 and the others fall as well (tho barely for IM)

# The problems of variance inflation and bias due to excluded regressors both involve correlated regressors.
# However there are methods, such as factor analysis or principal componenent analysis, which can convert
# regressors to an equivalent uncorrelated set. Interpretation may become difficult.




