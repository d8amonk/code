set.seed(21821)
ncust <- 1000

# build out a df of customer data
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))
cust.df$age <- rnorm(n = ncust, mean = 35, sd = 5)
cust.df$credit.score <- rnorm(n = ncust, mean = 3*cust.df$age+620, sd = 50)
cust.df$email <- factor(sample(c("yes","no"), size = ncust, replace = TRUE, prob = c(0.8, 0.2)))
# exp(rnorm(...)) == lognormal distribution rlnorm(...)
cust.df$distance.to.store <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))

# create data for the online store
# start with 15 online visits, add 15 if there's an email address on file, and simulate more visits for younger ages
cust.df$online.visits <- rnbinom(ncust, size = 0.3, mu = 15 + ifelse(cust.df$email == "yes", 15,0) - 0.7*(cust.df$age - median(cust.df$age)))

# assm a 30% chance of a customer making an online purchase given a visit; spend follows lognormal dist
cust.df$online.trans <- rbinom(ncust, size = cust.df$online.visits, prob = 0.3)
cust.df$online.spend <- exp(rnorm(ncust, mean = 3, sd = 0.1)) * cust.df$online.trans

# generate in-store sales data. assm visit == sale (tracking).
# negative binomial == lower average number of visits for cust living further away
cust.df$store.trans <- rnbinom(ncust, size = 5, mu = 3/sqrt(cust.df$distance.to.store))
cust.df$store.spend <- exp(rnorm(ncust, mean = 3.5, sd=0.4)) * cust.df$store.trans

# inspect
summary(cust.df)

# create sim'd survey data 
sat.overall <- rnorm(ncust, mean = 3.1, sd = 0.7)

sat.service <- floor(sat.overall + rnorm(ncust, mean = 0.5, sd = 0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean = -0.2, sd = 0.6))

summary(cbind(sat.service, sat.selection))
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))

# model non-response
no.response <- as.logical(rbinom(ncust, size = 1, prob = cust.df$age/100))
sat.service[no.response] <- NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))

cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
rm(ncust, sat.overall, sat.service, sat.selection, no.response)

# exploration
plot(cust.df$age, cust.df$credit.score,
     col = "blue",
     xlim = c(15,55), ylim = c(500, 900),
     main = "Active Customer as of MMM YYYY",
     xlab = "Customer Age (in Years)", ylab = "Customer Credit Score")
abline(h = mean(cust.df$credit.score), col = 'dark blue', lty = 'dotted')
abline(v = mean(cust.df$age), col = 'dark blue', lty = 'dotted')

plot(cust.df$store.spend, cust.df$online.spend,
     main = 'Customers as of MM YYYY',
     xlab = 'Prior 12 months in-store sales ($)',
     ylab = 'Prior 12 months online sales ($)',
     cex = 0.7)
# skewed data doesn't really give insight, try
hist(cust.df$store.spend,
     breaks = (0:ceiling(max(cust.df$store.spend)/10))*10,
     main = 'Customers as of MM YYYY',
     xlab = 'Prior 12 months online sales($)',
     ylab = 'Count of Customers')

# color by email(0,1)
my.col <- c('black','green3')
my.pch <- c(1, 19)

# index email factor to color
my.col[as.numeric(head(cust.df$email))]

# repeat skewed graph
plot(cust.df$store.spend, cust.df$online.spend,
     cex = 0.7,
     col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     main = 'Customers as of MM YYYY',
     xlab = 'Prior 12 months in-store sales ($)',
     ylab = 'Peior 12 months online sales ($)')
# log xy for insight
plot(cust.df$store.spend + 1, cust.df$online.spend + 1,
     log = 'xy', cex = 0.7,
     col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     main = 'Customers as of MM YYYY',
     xlab = 'Prior 12 months in-store sales ($)',
     ylab = 'Peior 12 months online sales ($)')
legend(x = 'topright', legend = paste("E-Mail on file:", levels(cust.df$email)), col = my.col, pch = my.pch)

par(mfrow = c(2,2))
plot(cust.df$distance.to.store, cust.df$store.spend, main = 'Store')
plot(cust.df$distance.to.store, cust.df$online.spend, main = 'Online')
plot(cust.df$distance.to.store, cust.df$store.spend, log = 'xy', main = 'Store, Logged')
plot(cust.df$distance.to.store, cust.df$online.spend, main = 'Online, Logged')

# build a pairs formula
pairs(formula = ~ age + credit.score + email + distance.to.store + online.visits + online.trans + online.spend + store.trans + store.spend, data = cust.df)
# or
pairs(cust.df[, c(2:10)])
# or
library(car)
scatterplotMatrix(formula = ~ age + credit.score + email + distance.to.store + online.visits + online.trans + online.spend + store.trans + store.spend, 
                  data = cust.df, diagonal = 'histogram')
scatterplotMatrix(cust.df[, 9:10])
# or
library(gpairs)
gpairs(cust.df[, c(2:10)])

# analysis
# cov vs cor
cov(cust.df$age, cust.df$credit.score)
cor(cust.df$age, cust.df$credit.score)
# note that cor = cov(xy)/(sd(x)sd(y))
cov(cust.df$age, cust.df$credit.score) / (sd(cust.df$age)*sd(cust.df$credit.score))
# is this cor significant?
cor.test(cust.df$age, cust.df$credit.score)

# multiple correlations
cor(cust.df[,c(2,3,5:12)], use = "complete.obs")
# easier
library(corrplot)
library(gplots)
corrplot.mixed(corr = cor(cust.df[, c(2,3,5:12)], use = "complete.obs"),
               upper = "ellipse", tl.pos = 'lt',
               col = colorpanel(50, 'red', 'gray60', 'blue4'))

# note
set.seed(49931)
x <- runif(1000, min = -10, max = 10)
cor(x, x^2)
plot(x, x^2)
