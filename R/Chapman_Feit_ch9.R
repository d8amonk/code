require(gpairs)
options(scipen = 999)

cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

spend.m1 <- lm(online.spend ~ ., data = subset(cust.df[, -1], online.spend > 0))
summary(spend.m1)
# high r^2, online.visits v. online.trans?

gpairs(cust.df)

autoTransform <- function(x){
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0) #145 drops
numcols <- which(colnames(cust.df.bc) != 'email')

cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform)
summary(cust.df.bc)
gpairs(cust.df.bc)

spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)
summary(spend.m2) #no better than one that just uses number of transactions
spend.m3 <- lm(online.spend ~ online.trans, data = cust.df.bc)
anova(spend.m3, spend.m2)

require(car)
vif(spend.m1) # vif > 5.0 == remediate collinearity 
vif(spend.m2) # vif > 5.0 == remediate collinearity 

spend.m4 <- lm(online.spend ~ . - online.trans - store.trans, data = cust.df.bc)
vif(spend.m4)
summary(spend.m4)

pc.online <- prcomp(cust.df.bc[ , c('online.visits', 'online.trans')])
cust.df.bc$online <- pc.online$x[ , 1]
pc.store <- prcomp(cust.df.bc[ , c('store.trans', 'store.spend')])
cust.df.bc$store <- pc.store$x[, 1]

spend.m5 <- lm(online.spend ~ email + age + credit.score + 
                              distance.to.store + sat.service +
                              sat.selection + online + store,
                              data = cust.df.bc)
summary(spend.m5)
vif(spend.m5)




# play with logits
exp(0) / (exp(0) + 1) #0.5
plogis(-Inf)
plogis(2) # log(0.88 / (1-0.88))
plogis(-0.2)
log(0.88 / (1-0.88)) # plogis(2)
qlogis(0.88) # plogis(2)

# season pass data
pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))
summary(pass.df)
table(pass.df$Channel, pass.df$Promo)

pass.tab <- c(242,639,38,359,284,27,449,223,83,278,49,485)
dim(pass.tab) <- c(3,2,2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel = c("Mail", "Park", "Email"),
                           Promo = c("Bundle", "NoBundle"),
                           Pass = c("YesPass", "NoPass"))

require("vcdExtra")
pass.df <- expand.dft(pass.tab)
str(pass.df)
table(pass.df$Pass, pass.df$Promo)
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))

pass.m1 <- glm(Pass ~ Promo, data = pass.df, family = binomial)
summary(pass.m1)

plogis(0.3888)/(1-plogis(0.3888)) # or
exp(0.3888) # so
exp(coef(pass.m1))
exp(pass.m1$coefficients)
exp(confint(pass.m1)) #28.2 to 69.7% more likely to buy your product if it's bundled
doubledecker(table(pass.df))
# we need to account for bias from park sales
# and emailers more likely to bundle
pass.m2 <- glm(Pass ~ Promo + Channel, data = pass.df, family = binomial)
summary(pass.m2) #promo is negative!

exp(coef(pass.m2))
exp(confint(pass.m2))


