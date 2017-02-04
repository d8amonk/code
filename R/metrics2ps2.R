#recommend stepping through with ctrl-R

setwd("E:/School/Metrics 2/Ps2")
waged <- read.csv("CPS_06.csv")

#these don't work yet
# pdf("wage_dataset.pdf")
# fix(waged)
# #you must manually break with esc in console then run
# dev.off()
# #go look for the pdf of your dataset in your wd

View(waged)
plot(waged$wage, waged$educ)
attach(waged)
#so you don't have to subset the whole time

plot(wage, educ)
plot(educ,wage)
#notice the order of x, y is important; type >plot 

#here the order of y, x is reversed 
mean(educ)
lm(wage~educ)
plot(educ, wage)
lm.fit<-lm(educ ~ wage)
lm.fit
summary(lm.fit)
confint(lm.fit)
plot(educ,wage)
plot(educ ,wage ,col ="red ")
plot(educ ,wage ,pch =20)
plot(educ ,wage ,pch ="+")
abline(lm.fit)
abline (lm.fit ,lwd =3)
abline (lm.fit ,lwd =3, col ="red ")

#this is pretty cool, "zoom" in on certain interactions by deleting vars:
pairs(~ educ + jc + univ + exper + female, waged)
pairs(~wage + nonwhite + services + profocc + union , waged)
#ctrl-R
g <- gl(n=9,k=1379, labels= c(9:18)) #9:18 for years of educ
str(g)
plot(educ,wage, type = "n")

points(educ[g==1], wage[g==1], col = "Green", pch = 17)
points(educ[g==2], wage[g==2], col = "Blue", pch = 18)
points(educ[g==3], wage[g==3], col = "Red", pch = 19)
points(educ[g==4], wage[g==4], col = "Purple", pch = 20)
points(educ[g==5], wage[g==5], col = "Orange", pch = 21)
points(educ[g==6], wage[g==6], col = "Blue", pch = 22)
points(educ[g==7], wage[g==7], col = "Green", pch = 23)
points(educ[g==8], wage[g==8], col = "Red", pch = 24)
points(educ[g==9], wage[g==9], col = "Black", pch = 25)


#or much more simply, library(ggplot2) and
qplot(educ, wage, data = waged, color = g, geom = c("point","smooth"), method = "lm")

#####################################################
#this is where i get lazy and sub educ=x and wage=y##
#and use generic functions from coursera, some of####
#these you'll have to change the NAMED variables#####
#####################################################
x<-educ
y<-wage

#lattice with options in function, median(y) dashed line
xyplot(y ~ x | g,
       panel = function(x,y,...){
         panel.xyplot(x,y,...)
         panel.abline(h = median(y),
                      lty = 5,
                      col = 2)})

#REGRESSION LINE, lattice with options in function
xyplot(y ~ x | g,
       panel = function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,col=2)
       })

qplot(x, y, data = waged, color = educ, geom = c("point","smooth"), method = "lm")

#shingled intervals
library(lattice)
#library(help = lattice)

head(waged)
xyplot(wage ~ educ, data = waged)
female.cut <- equal.count(waged$female==1, 4)
male.cut <- equal.count(waged$female==0, 4)


xyplot(wage ~ educ | 
         female.cut * male.cut,
       data=waged, 
       #layout=c(1,4), 
       as.table=TRUE,
       pch = 20,
       panel = function(x, y,...){
         panel.xyplot(x,y,...)
         fit <- lm(y~x)
         #panel.abline(fit,col=2)
         panel.loess(x,y,col=2,lwd=5)
       },
       xlab = "years of education",
       ylab = "wage",
       main = "wage on education")


#HISTOGRAM examples
histogram(~ educ | female.cut * male.cut, data = waged)
#hist using qplot
qplot(educ, data=waged, fill = wage)
qplot(educ, data = waged)
qplot(educ, data = waged, fill = wage)

#examples of QPLOT
library(ggplot2)
#str(mpg)
#qplot(displ,hwy,data=mpg)
#qplot(displ,hwy,data=mpg, color = drv)
#qplot(displ,hwy,data=mpg, color = drv, geom = c("point", "smooth"))
#qplot(displ,hwy,data=mpg, geom = c("point", "smooth"))
#geom = "density"

#FACETS
# facets = row ~ column
#dont use this (Crash): qplot(x=educ, data=waged, facets = wage ~., binwidth =2)
qplot(displ, hwy, data=mpg, facets = .~drv)
qplot(hwy, displ, data = mpg, geom = c("point","smooth"),method = "lm", facets = .~drv)



#Density Plots
#this is a density plot 
qplot(educ, data = waged, geom = "density")
#this is a density plot of the x, by color = type
##doesn't work qplot(educ, data = mpg, geom = "density", color = 2)


#scatterplots
qplot(wage, educ, data = waged)
#qplot(wage, educ, data = waged, shape = female)
qplot(wage, educ, data = waged, color = female)

#ggplot is additive!! 
#g <- ggplot(data=waged, color = female,) #,shape = drv)
# g + geom_smooth
# g + geom_smooth(method = "lm")
# #good for separating categorical vars
# g + geom_smooth(method = "lm") + facet_grid(drv~.) + geom_point(color="steelblue", size = 4, alpha =1/2)
# g + geom_smooth(size =2, linetype= 4, method = "lm") + geom_point(aes(color=drv), size = 4, alpha =1/4) #here, color is variable

#Making Tertiles
# cutpoints <- quantile(dataset$var1, seq(0,1, length=4), na.rm=TRUE)
# dataset$var2 <- cut(dataset$var1, cutpoints)
# levels(datasets$var2)
cutpoints <- quantile(mpg$displ, seq(0,1, length=4), na.rm=TRUE)
mpg$newcyl <- cut(mpg$displ, cutpoints)
levels(mpg$newcyl)

## Setup ggplot with data frame
# g <- ggplot(dataset, aes(var1, var2))
#   
#   ## Add layers!
#   g + geom_point(alpha = 1/3)
#   + facet_wrap(var1 ~ var2, nrow = 2, ncol = 4)
#   + geom_smooth(method="lm", se=FALSE, col="steelblue")
#   + theme_bw(base_family = "Avenir", base_size = 10)
#   + labs(x = expression("log " * ?plotx[subscript]
#   + labs(y = "whatever")
#                   + labs(title = "xyz")
