par(pch=, lty=, lwd=, col=, las=, bg=, mar=, oma=, mfrow=, mfcol=)
# pch = plotting symbol (def: cirlce)
# lty = line type (def: solid)
# lwd = line width (integer)
# col = color
# las = label orientation (1,2)
# bg = background color
# mar= margin size (def: 0)
# oma= outer margin size 
# mfrow = num of plots per row, column ## plots are filled row-wise
# mfcol = num of plots per row, column ## plots are filled column-wise
par("oma")
#following functions ADD to an EXISTING PLOT
plot
lines
points
text
title
mtext
axis

#subsetting based on levels for graphing
m <- rnorm(100)
f <- rnorm(100)
g <- gl(2,50, labels = c("Male", "Female"))
str(g)
plot(x,y, type = "n")
points(x[g=="Male"], y[g=="Male"], col = "Green", pch = 19)
points(x[g=="Female"], y[g=="Male"], col = "Blue", pch = 20)

#lattice functions with panel function
x<-rnorm(100)
y <- x + rnorm(100, sd=0.5)
f <- gl(2, 50, labels = c("Group 1", "Group 2"))
xyplot(y~x|f)

#lattice with options in function, median(y) dashed line
#f <- gl(2, 50, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f,
  panel = function(x,y,...){
    panel.xyplot(x,y,...)
    panel.abline(h = median(y),
      lty = 2)
    
    
  })

#REGRESSION LINE, lattice with options in function
xyplot(y ~ x | f,
       panel = function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,col=2)
        })

qplot(hwy, displ, data = mpg, color = drv, geom = c("point","smooth"), method = "lm")

#shingled intervals
library(lattice)
library(help = lattice)
data(environmental)
head(environmental)
xyplot(ozone ~ radiation, data = environmental)
temp.cut <- equal.count(environmental$temperature,4)
wind.cut <- equal.count(environmental$wind, 4)


xyplot(ozone ~ radiation | 
       temp.cut * wind.cut,
       data=environmental, 
       #layout=c(1,4), 
       as.table=TRUE,
       pch = 20,
       panel = function(x, y,...){
         panel.xyplot(x,y,...)
         fit <- lm(y~x)
         #panel.abline(fit,col=2)
         panel.loess(x,y,col=1,lwd=2)
      },
       xlab = "Solar Radiation",
       ylab = "Ozone (ppb)",
       main = "Ozone vs. Solar Radiation")


#SPLOM function
splom(~ environmental)

#HISTOGRAM examples
histogram(~ ozone | wind.cut * temp.cut, data = environmental)
#hist using qplot
qplot(hwy, data=mpg, fill = drv)
qplot(hwy, data = mpg)
qplot(hwy, data = mpg, fill = drv)

#examples of QPLOT
library(ggplot2)
#str(mpg)
qplot(displ,hwy,data=mpg)
qplot(displ,hwy,data=mpg, color = drv)
qplot(displ,hwy,data=mpg, color = drv, geom = c("point", "smooth"))
qplot(displ,hwy,data=mpg, geom = c("point", "smooth"))
#geom = "density"

#FACETS
# facets = row ~ column
qplot(hwy, data=mpg, facets = drv ~., binwidth =2)
qplot(displ, hwy, data=mpg, facets = .~drv)
qplot(hwy, displ, data = mpg, geom = c("point","smooth"),method = "lm", facets = .~drv)



#Density Plots
#this is a density plot of the hwy mileage
qplot(hwy, data = mpg, geom = "density")
#this is a density plot of the hwy mileage, by drv type
qplot(hwy, data = mpg, geom = "density", color = drv)
 

#scatterplots
qplot(hwy, displ, data = mpg)
qplot(hwy, displ, data = mpg, shape = drv)
qplot(hwy, displ, data = mpg, color = drv)
#BONUS: knock yourself out and use color and shape
qplot(hwy, displ, data = mpg, color = drv, shape = drv)

#ggplot is additive!! 
g <- qplot(hwy, displ, data = mpg, color = drv, shape = drv)
g + geom_smooth
g + geom_smooth(method = "lm")
#good for separating categorical vars
g + geom_smooth(method = "lm") + facet_grid(drv~.) + geom_point(color="steelblue", size = 4, alpha =1/2)
g + geom_smooth(size =2, linetype= 4, method = "lm") + geom_point(aes(color=drv), size = 4, alpha =1/4) #here, color is variable

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

