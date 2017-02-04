# install.packages('Rcpp')
library(devtools)
# install_github('google/CausalImpact')
library(CausalImpact)

set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 1000)

y <- 1.2*x1 + rnorm(1000)
# create intervention effect @ t= 71
y[710:1000] <- y[710:1000] + 10
data <- cbind(y, x1)

dim(data)
plot(data)
head(data)
matplot(data, type = 'l')

# pre- and post-intervention periods
pre.period <- c(1,709)
post.period <- c(710,1000)

impact <- CausalImpact(data, pre.period, post.period)
summary(impact)
summary(impact, 'report')

plot(impact) 
# 'all of the above inferences depend critically on 
# the assumption that the covariates were not themselves 
# affected by the intervention. The model also assumes 
# that the relationship between covariates and treated 
# time series, as established during the pre-period, 
# remains stable throughout the post-period.'

# ?zoo
time.points <- seq.Date(as.Date('2014-01-01'), by = 1, length.out = 1000)
data <- zoo(cbind(y, x1), time.points)
head(data)

pre.period <- as.Date(c('2014-01-01', '2015-12-10'))
post.period <- as.Date(c('2015-12-11', '2016-09-26'))
# indices are now dates
impact <- CausalImpact(data, pre.period, post.period)
plot(impact) 

# construct a custom model
library(bsts)

# need to mask the counterfactual outcomes after intervention. As before...
post.period <- as.Date(c('2015-12-11', '2016-09-26'))
# preserve the outcomes
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA

ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ x1, ss, niter = 1000)

impact <- CausalImpact(bsts.model = bsts.model, 
                       post.period.response = post.period.response)
plot(impact)
summary(impact)
summary(impact, 'report')

# customize plot
plot(impact, c("original", "pointwise"))

# use gg to change (font)
library(ggplot2)
library(ggthemes)
impact.plot <- plot(impact) + theme_solarized(base_size = 20)
plot(impact.plot)


