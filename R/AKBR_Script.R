library(statsr)
library(MASS)
library(dplyr)
library(ggplot2)
library(BAS)

data(wage)

summary(wage$wage)

ggplot(wage, aes(wage))+
  geom_histogram(bins = 100)

m_wage_iq = lm(wage ~ iq, data = wage)
m_wage_iq$coefficients

summary(m_wage_iq)$sigma

m_wage_ed = lm(wage ~ educ, data = wage)
m_wage_ed$coefficients

summary(m_wage_ed)$sigma

ggplot(wage, aes(m_wage_iq$residuals))+
  geom_density() #super useful

(m_lwage_iq = lm(lwage ~ iq, data = wage))

confint(m_lwage_iq)

m_lwage_full = lm(lwage ~ . - wage, data = wage)

summary(m_lwage_full)

BIC(m_lwage_full)

m_lwage_nobrthord = lm(lwage ~ . -wage -brthord, data = na.omit(wage))
BIC(m_lwage_nobrthord)

m_lwage_nofeduc = lm(lwage ~ . -wage -feduc, data = na.omit(wage))
BIC(m_lwage_nofeduc)

wage_no_na = na.omit(wage)
bma_lwage = bas.lm(lwage ~ . -wage, data = wage_no_na,
                   prior = "BIC", 
                   modelprior = uniform())
bma_lwage
summary(bma_lwage)

par(mfrow = c(1,2))
coef_lwage = coefficients(bma_lwage)
plot(coef_lwage, subset = c(3,13), ask=FALSE)

confint(coef_lwage)

wage_red = wage %>%
  select(-sibs, -brthord, -meduc, -feduc)

bma_wage_red = bas.lm(lwage ~ . -wage, data = wage_red,
                      prior = "BIC",
                      modelprior = uniform())
bma_wage_red
summary(bma_wage_red, n.models = 100)
plot(bma_wage_red, subset = "age", ask=FALSE)

set.seed(314)
N = 100000
phi = rgamma(N,2,2)
sigma2 = 1/phi
mu = rnorm(N, 1, sqrt(sigma2)/4)
y = rnorm(N, mu, sqrt(sigma2))

BPM_pred_lwage =  predict(bma_lwage, estimator="BPM", se.fit=TRUE)
bma_lwage$namesx[BPM_pred_lwage$bestmodel + 1]

MPM_pred_lwage =  predict(bma_lwage, estimator="MPM")
bma_lwage$namesx[MPM_pred_lwage$bestmodel + 1]

opt = which.max(BPM_pred_lwage$fit)
t(wage_no_na[opt, ])

ci_lwage = confint(BPM_pred_lwage, parm="pred")
ci_lwage[opt,]

exp(ci_lwage[opt,])

BMA_pred_lwage =  predict(bma_lwage, estimator="BMA", se.fit=TRUE)
ci_bma_lwage = confint(BMA_pred_lwage, estimator="BMA")
opt_bma = which.max(BMA_pred_lwage$fit)
exp(ci_bma_lwage[opt_bma,])
