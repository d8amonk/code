setwd("C:/Users/Jeffrey/Desktop")
Auto = read.table("Auto.txt",header=T,na.strings="?")
attach(Auto)

cylinders=as.factor(cylinders)

plot(cylinders,mpg,col="red",varwidth=T, horizontal = T)

hist(mpg,col=2,breaks=15)

#rooting around...
pairs(Auto)
pairs (~ mpg + displacement + horsepower +  weight + acceleration, Auto)
plot(displacement,weight)
identify(displacement,weight,name)

summary(displacement)

summary(Auto)
college = read.csv("college.csv")
plot(Private, Accept)
Private = as.numeric(Private)
plot(Private, Accept)
summary(Accept)
plot(Apps,Grad.Rate)
rownames(college) = college[,1]
college = college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
Elite <- rep("No",nrow(college))
Elite <- rep("No",nrow(college))
Elite[Top10perc>50]="Yes"
college = data.frame(college,Elite)
fix(college)
boxplot(Outstate,Elite)

#load MASS, ISLR
library(MASS)
library(ISLR)
attach(Boston)
lm.fit  <- lm(medv~lstat)
summary(lm.fit)
lm.fit$model
l <- lm.fit
confint(l)

predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "confidence")

predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "prediction")

plot(lstat,medv,pch="+")
abline(lm.fit, lwd = 5, col = "red")

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit)) #slightly smaller

#leverage stats
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit = lm(medv~lstat+age)
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
#RSE
summary(lm.fit)$sigma

#variance inflation factor
library(car)
vif(lm.fit)

#run reg on all but one (age has a hi p ^ above)
lm.fit1 = lm(medv~.-age,data=Boston)
summary(lm.fit1)
#alt: lm..fit1 = update(lm.fit,~.-age)

summary(lm(medv~lstat*age))
#same as 
#summary(lm(medv~lstat + age + lstat:age))

#non-lin xforms
lm.fit2=lm(medv~lstat+I(lstat^2))

summary(lm.fit2)
summary(lm(medv~lstat)) #compare
plot(medv,lstat)

#compare different models
plot(medv,(lstat^2))
lm.fit = lm(medv~lstat)
anova(lm.fit,lm.fit2)
#the quadradtic fit is far superior to the linear fit
# The anova() function performs a hypothesis test comparing the two models. The null hypothesis is that the two models fit the data equally well, and the alternative hypothesis is that the full model is superior. Here the F-statistic is 135 and the associated p-value is virtually zero. 

#compare the patterns in the resids:

plot(lm.fit2)

#5-ord polynomial form of lstat:anova(lm.fit2,lm.fit5)
lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)

#wayyy better - but none beyond 5 add much to the ESS
anova(lm.fit2,lm.fit5)

#try a log fit
summary(lm(medv~log(lstat)))
lm.fitln = lm(medv~log(lstat))
anova(lm.fit5,lm.fitln)
#not better than a 5th-order
lm.fit4 = lm(medv~poly(lstat,4))
anova(lm.fit4,lm.fitln)
#this is just about (**) where they're even. that is, a lin-log fits the model just as well as a 4o-poly lin-lin.finally do a visual inspection of the resids to see they're almost the same
plot(lm.fit4)

plot(lm.fitln)

#using Carseats dataset
fix(Carseats)
lm.fit <- lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
summary(lm.fit)

#dummy values
attach(Carseats)
contrasts(ShelveLoc)

#logistic regression -- Smarket data set 
attach(Smarket)
plot(Volume)

#glm model, specify type of linear model to fit
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)

attach(Smarket)
plot(Volume)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
#detailed version (summary(glm.fit)$coef) is same as typing summary(glm.fit)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)

#confusion matrix
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.50]="Up"
#check for accuracy
table(glm.pred,Direction)
(507+145)/1250 #same as
mean(glm.pred==Direction)

#build train and holdout subsets *training error will always underestimate test error
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

#test the training model on the test sample
glm.fit=glm(Direction???Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=Smarket ,family =binomial ,subset =train )

#compute predictions
glm.pred=rep ("Down " ,252)
glm.pred[glm.probs >.5]=" Up"

table(glm.pred ,Direction.2005) #PROBLEMS
mean(glm.pred== Direction.2005)
