require(ggplot2)

# binomial test - Butterflies moving north
library(BayesianFirstAid)
binom.test(c(9,2)) # not as useful as
fit <- bayes.binom.test(c(9,2))
summary(fit)
# change the reference value and con
bayes.binom.test(9, 11, conf.level = 0.8, p = 0.75)
# adjust the precision of the MCMC by increasing iters
# bayes.binom.test(c(9, 2), n.iter = 999999)

fit <- bayes.binom.test(c(9, 2))
plot(fit)
summary(fit)
diagnostics(fit)

model.code(fit)
# in order to change the flat prior on theta into Jeffrey's 
# prior we simply change dbeta(1, 1) into dbeta(0.5, 0.5) and rerun the code:
# x <- 9
# n <- 11
# 
# model_string <-"model {
#   x ~ dbinom(theta, n)
#   theta ~ dbeta(0.5, 0.5)
#   x_pred ~ dbinom(theta, n)
# }"
# 
# model <- jags.model(textConnection(model_string), data = list(x = x, n = n),
#                     n.chains = 3, n.adapt=1000)  
# samples <- coda.samples(model, c("theta", "x_pred"), n.iter=5000)
# 
# summary(samples)

# paired sample t-test
require(readr)

url <- "http://www.sumsar.net/files/posts/2014-02-04-bayesian-first-aid-one-sample-t-test/roubik_2002_coffe_yield.csv"
str(d <- read_csv(url))

# compare new world yield pre- and post-African honeybe
new_yield_80 <- d$yield_61_to_80[d$world == "new"]
new_yield_01 <- d$yield_81_to_01[d$world == "new"]
t.test(new_yield_01, new_yield_80, paired = TRUE, alternative = "greater")
# unhelpful confint, use Bayes credint

library(BayesianFirstAid)
# compare new world yield pre- and post-African honeybe
# you increase the number of MCMC iterations by using the 
# argument n.iter when calling bayes.t.test.
fit_new <- bayes.t.test(new_yield_01, new_yield_80, paired = TRUE)

# compare new world yield pre- and post-African honeybe
old_yield_80 <- d$yield_61_to_80[d$world == "old"]
old_yield_01 <- d$yield_81_to_01[d$world == "old"]
fit_old <- bayes.t.test(old_yield_01, old_yield_80, paired = TRUE)

plot(fit_new)
plot(fit_old)
summary(fit_new)
summary(fit_old)

model.code(fit_new)
# If we believe, for example, that robustness is not such a big issue 
# and would like to assume that the data is normally distributed rather 
# than t distributed we just have to make some small adjustments to 
# this script. In the model code we change dt into dnorm and remove 
# the nu parameter resulting in this model:
model_string <- "model {
  for(i in 1:length(pair_diff)) {
    pair_diff[i] ~ dnorm( mu_diff , tau_diff)
  }
  diff_pred ~ dnorm( mu_diff , tau_diff)
  eff_size <- (mu_diff - comp_mu) / sigma_diff

  mu_diff ~ dnorm( mean_mu , precision_mu )
  tau_diff <- 1/pow( sigma_diff , 2 )
  sigma_diff ~ dunif( sigmaLow , sigmaHigh )
}"
# The parameters to monitor.
params <- c("mu_diff", "sigma_diff", "eff_size", "diff_pred")

# two-sample t-test
url <- "http://www.sumsar.net/files/posts/2014-02-04-bayesian-first-aid-one-sample-t-test/roubik_2002_coffe_yield.csv"
str(d <- read_csv(url))
yield_diff_new <- with(d[d$world == "new", ], yield_81_to_01 - yield_61_to_80)
yield_diff_old <- with(d[d$world == "old", ], yield_81_to_01 - yield_61_to_80)

ggplot(d, aes(x = world, y = yield_81_to_01 - yield_61_to_80)) + geom_boxplot()

(fit <- bayes.t.test(yield_diff_new, yield_diff_old))
summary(fit)
plot(fit)

# there is little evidence of a difference in means between New and 
# Old World type countries, there is some evidence for a difference 
# in SD (check out the posterior of sigma_diff)

summary(fit)
# diagnostics will give us an equally long list of MCMC 
# convergence diagnostics and traceplots. 

# Pearson Correlation Test
# the working hypothesis is that the 2D:4D ratio is a proxy variable 
# for prenatal androgen exposure and could therefore be related to a 
# host of other traits related to "manliness" such as aggression, 
# prostate cancer risk, sperm count, etc.
url <- "http://sumsar.net/files/posts/2014-03-17-bayesian-first-aid-pearson-correlation-test/2d4d_hone_2012.csv"
d <- read.csv(url)

qplot(ratio_2d4d, grip_kg, data = d, shape = I(1)) + 
  facet_grid(sex ~ ., scales = "free")

cor.test( ~ ratio_2d4d + grip_kg, data = d[d$sex == "male", ])
cor.test( ~ ratio_2d4d + grip_kg, data = d[d$sex == "female", ])

fit_male <- bayes.cor.test( ~ ratio_2d4d + grip_kg, data = d[ d$sex == "male",])
fit_male

plot(fit_male)
# At the top we have the posterior distribution for the correlation rho with
# a 95% highest density interval. At the bottom we see the original data with 
# superimposed posterior predictive distributions (that is, the distribution
# we would expect a new data point to have). This is useful when assessing 
# how well the model fits the data. The two ellipses show the 50% (darker blue) 
# and 95% (lighter blue) highest density regions. The red histograms show the 
# marginal distributions of the data with a smatter of marginal densities drawn 
# from the posterior. Looking at this plot we see that the model fits quite well, 
# however, we could be concerned with the right skewness of the ratio_2d4d marginal 
# which is not captured by the model.
fit_female <- bayes.cor.test( ~ ratio_2d4d + grip_kg, data = d[ d$sex == "female",])
plot(fit_female)

# Indeed, 2D:4D ratio and strength seems to be slightly less correlated than for 
# the male group, but to claim there is evidence for no correlation seems a bit 
# unfounded. We could, however, take a look at the posterior difference in 
# correlation between the male and the female group. To do this we first extract 
# the MCMC samples from the Bayesian First Aid fit object using the as.data.frame 
# function.
female_mcmc <- as.data.frame(fit_female)
male_mcmc <- as.data.frame(fit_male)
head(female_mcmc)

# plot the difference in ???? and calculate the probability that rho is more 
# negative for men:
hist(male_mcmc$rho - female_mcmc$rho, 30, xlim = c(-1, 1), main = "", yaxt = "n")

mean(male_mcmc$rho - female_mcmc$rho < 0)

# Test of Proportions  
# Below, the data from the right handers are on the right, logical right? :)
n_right_leaners <- c(43, 275)
n_respondents <- c(170, 1454)
prop.test(n_right_leaners, n_respondents)

bayes.prop.test(n_right_leaners, n_respondents)
fit <- bayes.prop.test(n_right_leaners, n_respondents)
plot(fit)
# it is most likely that left-handers lean more to the 
# right by around 6-7 percentage points

# embryos (30s v 90m)
no_good_grade <- c(134, 152)
no_embryos <- c(228, 225)
fit <- bayes.prop.test(no_good_grade, no_embryos)
plot(fit)














