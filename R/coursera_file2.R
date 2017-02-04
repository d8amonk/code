p <- seq(from = 0.1, to = 0.9, by = 0.1)

prior <- c(rep(0.06,4), 0.52, rep(0.06,4))

likelihood <- dbinom(4, size = 20, prob = p)

# calculating the posterior
numerator <- prior*likelihood # the vector of prior probabilities & data|model
denominator <- sum(numerator) # sum of all possbible
posterior <- numerator/denominator
sum(posterior)

barplot(prior)
barplot(likelihood)
barplot(posterior)

# bayesian paradigm allwos us to make direct probability statements about the models
# "what is the probability that the treament (RU-486) is more effective than the control?
sum(dbinom(4, size = 20, prob = p[p < 0.5]))


# pQuiz5
p <- c(0.2,0.4,0.5,0.6,0.8)
prior <- c(rep(0.125, 2), 0.5, rep(0.125,2))
likelihood <- dbinom(1, size = 3, prob = p)

numerator <- prior*likelihood # the vector of prior probabilities & data|model
denominator <- sum(numerator) # sum of all possbible
posterior <- numerator/denominator
sum(posterior)


library(sos); findFn("pgumbel") 
library(evd) # eg

x <- seq(-3, 3, length=100)
Fx = pgumbel(x,loc=0,scale=1)
y = dbeta(Fx, shape1=3,shape2=1)
par(las=1,bty="l")  ## my personal preferences
plot(x, y, type="l", lty=2, xlab="x value", ylab="Density", ylim=c(0,40))
