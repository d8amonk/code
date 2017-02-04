libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("aod","gridExtra","data.table", "scales","dplyr", "ggplot2", "ggthemes"))
rm(libraries)
options(scipen=999, digits = 3)



mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

glimpse(mydata)
summary(mydata)
sd_mydata <- sapply(mydata,sd); sd_mydata

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, mydata)

mydata$rank <- as.factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank,
               data = mydata,
               family = "binomial")
summary(mylogit)

confint(mylogit)
confint.default(mylogit)

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6) # The chi-squared test statistic of 20.9, with three degrees of freedom is associated with a p-value of 0.00011 indicating that the overall effect of rank is statistically significant.

# odds ratios (remember that coefficients are LOG ODDS!)
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4))); newdata1

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response"); newdata1

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100)))); newdata2  


newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE)); newdata3

newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
}); newdata3

## view first few rows of final dataset
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                                                                      size = 1)

# This can be particularly useful when comparing competing models
with(mylogit, null.deviance - deviance)


We may also wish to see measures of how well our model fits. 
This can be particularly useful when comparing competing models. 
The output produced by summary(mylogit) included indices of fit (shown below the coefficients), including the null and deviance residuals and the AIC. One measure of model fit is the significance of the overall model. This test asks whether the model with predictors fits significantly better than a model with just an intercept (i.e., a null model). The test statistic is the difference between the residual deviance for the model with predictors and the null model. The test statistic is distributed chi-squared with degrees of freedom equal to the differences in degrees of freedom between the current and the null model (i.e., the number of predictor variables in the model). To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:
  
  with(mylogit, null.deviance - deviance)
## [1] 41.5
The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the mode, and can be obtained using:
  
  with(mylogit, df.null - df.residual)
## [1] 5
Finally, the p-value can be obtained using:
  
  with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
## [1] 7.58e-08
The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. This is sometimes called a likelihood ratio test (the deviance residual is -2*log likelihood). To see the model's log likelihood, we type:

logLik(mylogit)
## 'log Lik.' -229 (df=6)