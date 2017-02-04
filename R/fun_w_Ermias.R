# Note: detach("package:xtable", unload=TRUE)

library(lubridate, quietly = T, warn.conflicts = F)
library(scales, quietly = T, warn.conflicts = F)
library(ggplot2, quietly = T, warn.conflicts = F)
library(ggthemes, quietly = T, warn.conflicts = F)
library(plyr, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(stargazer, quietly = T, warn.conflicts = F)

setwd("C:/Users/Jeffrey/Google Drive")
options(scipen = 999, digits = 2)

winners  <- read.csv("CDOT/Ermias/winners.csv")
exp  <- read.csv("CDOT/Ermias/exp.csv")
distinct_contracts <- read.csv("CDOT/Ermias/distinct_contracts_0014.csv")
distinct_winners <- join(winners, distinct_contracts, type = "left")
spendDiff  <- read.csv("CDOT/Ermias/spendDiff.csv")
spendOver  <- read.csv("CDOT/Ermias/spendOver.csv")
spendUnder <- read.csv("CDOT/Ermias/spendUnder.csv")

glimpse(winners)
glimpse(exp)
glimpse(distinct)
glimpse(distinct_winners)
# View(distinct_winners)
summary(distinct_winners[!complete.cases(distinct_winners),])
# Whats wrong with C20110?

attach(distinct_winners)
m1 <- lm(Award.Amount ~ num_payments)
m2 <- lm(Award.Amount ~ Vendor + num_payments)
m3 <- lm(Award.Amount ~ Vendor + length_days)
stargazer(m1,m2,m3, type = "html")

plot(start_month, Award.Amount)

plot(Letting.Date, Award.Amount, main = "Box Plots :: Award.Amount on Letting Date")

library(ggplot2)

g1 <- ggplot(distinct_winners, aes(x = Letting.Date, y = Award.Amount))
g1 + geom_point()

g2 <- ggplot(distinct_winners, aes(x = Award.Date, y = Award.Amount))
g2 + geom_point()
g2 + geom_line()

m4 <- lm(Award.Amount ~ Work.Type + Award.Date)

m5 <- lm(Award.Amount~Engineers.Estimate)
m6 <- lm(Award.Amount ~ length)
m7 <- lm(Award.Amount ~ (end-start))

length_days = end - start

m8 <- lm(Award.Amount ~ length_days)

m9 <- lm(log(Award.Amount) ~ len_months + num_payments + Region, data = distinct_winners)
m10 <- lm(Award.Amount ~ len_months + num_payments + Region, data = distinct_winners)

m11 <- lm(Award.Amount ~ Work.Type)

m12 <- lm(Award.Amount ~ tot) #XPI??
spendDiff <- tot - Award.Amount
spendUnder <- spendDiff[spendDiff < 0]
spendOver <- spendDiff[spendDiff > 0]

sum(spendDiff)
sum(spendOver)
sum(spendUnder)

lowBall <- which.min(spendUnder); lowBall
highBall <- which.min(spendOver); highBall
glimpse(distinct_winners[lowBall,]) #C18695

mean(spendDiff[spendDiff<0])
sum(x < 0)
plot(x)
plot(x < 0)
plot(distinct_winners[tot-Award.Amount < 0]
)
plot(x)

