options(scipen = 999)

setwd("c:/Users/jvangeete/Desktop/Asks/Heather/")
roku <- read.csv("Roku_GA_2.csv")
roku_0 <- read.csv("Roku_GA.csv")

roku$Date <- as.Date(roku$Date, format = "%m/%d/%Y")
roku_0$Date <- as.Date(roku_0$Date, format = "%m/%d/%Y")

summary(l1 <- lm(Activations ~ AvgSL, roku))
summary(l2 <- lm(Activations ~ AvgSL + Date, roku))
summary(l3 <- lm(Activations ~ AvgSL + Users + Medium, roku))

summary(l3 <- lm(Activations ~ AvgSL + Users + Medium, roku_0))





summary(l1.1 <- lm(Activations ~ AvgSL, roku_0))
summary(l2.1 <- lm(Activations ~ AvgSL + Date, roku_0))

anova(l1, l2)









