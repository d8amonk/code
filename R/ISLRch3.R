library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
lm.fit(medv~lstat, data=Boston)
lm.fit
summary(lm.fit)
lm.fit=lm(medv~lstat, data=Boston)
lm.fit
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
confint(lm.fit)
plot(lstat,medv)
abline(lm.fit)
abline (lm.fit ,lwd =3)
abline (lm.fit ,lwd =3, col ="red ")
plot(lstat ,medv ,col ="red ")
plot(lstat ,medv ,pch =20)
plot(lstat ,medv ,pch ="+")
#plot (1:20 ,1:20, pch =1:20)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma
lm.fit1=lm(medv~.-age ,data=Boston) # or, lm.fit1=update (lm.fit , ???.-age)
summary(lm.fit1)$r.sq
summary(lm.fit1)$sigma

summary (lm(medv???lstat *age ,data=Boston )) 
# * is s.h. for (x1*x2 = x1+x2+x1:x2) where x1:x2 is interaction term

