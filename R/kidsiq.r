library(BAS)
library(statsr)
library(foreign)
library(dplyr)

df <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/child.iq/kidiq.dta")
glimpse(df)

cog_bas <- bas.lm(kid_score ~ mom_hs + mom_iq + mom_work + mom_age, 
                  prior = "BIC",
                  modelprior = uniform(),
                  data = df)
round(summary(cog_bas), 3)
image(cog_bas, rotate = F)
