#random normal
# set.seed(20) #could be anything
# x <- rnorm(100)
# e <- rnorm(100,0,2)
# y <- 0.5 + 2*x + e
# summary(y)
# plot(x,y)

#random binary
#set.seed(10)
# x <-rbinom(100,1,0.5)
# e <-rnorm(100,0,2)
# y <- 0.5 + 2*x + e
# summary(y)
# plot(x,y)

#Poisson Model
#set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 +0.3*x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x,y)

#REGRESSION LINE, lattice with options in function
xyplot(y ~ x | f,
       panel = function(x,y,...){
         panel.xyplot(x,y,...)
         panel.lmline(x,y,col=2)
       })
