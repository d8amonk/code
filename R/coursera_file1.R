library(statsr)

m1_good_prior <- (1/2)
m2_good_prior <- (1/2)
m1_bad_prior <- (1/2)
m2_bad_prior <- (1/2)

winm1_m1good <- (1/2)
winm1_m1bad <- (1/3)
winm2_m2good <- (1/2)
winm2_m2bad <- (1/3)

losem1_m1good <- (1/2)
losem1_m1good <- (2/3)
losem2_m2good <- (1/2)
losem2_m2good <- (2/3)

#q1
(winm1_m1good*m1_good_prior)/sum(winm1_m1good*m1_good_prior, winm1_m1bad*m1_bad_prior)
#q2
(winm1_m2good*m1_good_prior)/sum(winm1_m1good*m1_good_prior, winm1_m1bad*m1_bad_prior)

bandit_posterior(data = data.frame(machine = c(1L,1L,2L,2L,2L), 
                                   outcome = c("W","W","W","W","L")))
bandit_posterior(data = data.frame(machine = c(2L,2L,2L,1L,1L), 
                                   outcome = c("W","W","L","W","W")))



curve(dbeta(x, 4, 22))
pbeta(.2, shape1 = 4, shape2 = 22)


#week 2 lab
library(statsr)
data(brfss)

n <- length(brfss$sex)
x <- sum(brfss$sex == "Female")
prior <- dbeta(x, 1, 1)
posterior <- dbeta(x, 2587, 2415)
5 + 2586 
200 + 2414
sum(as.integer(brfss$exercise))
length(brfss$exercise)
