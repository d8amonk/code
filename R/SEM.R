library(readxl)
library(dplyr)
library(GGally)
d <- read_excel("c:/Users/jvangeete/Desktop/Asks/MikeBroome_lytics.xlsx", sheet = 1)

d$IMPS <- as.numeric(d$IMPS)

plot(log(d$CLKS/d$IMPS), log(d$CONV_RATE))

sum(is.na(d))

l1 <- lm(log(d$CONV_RATE+0.01458) ~ log((d$CLKS+171.2)/(d$IMPS+1439.9)))
