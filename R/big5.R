library(readxl)
library(dplyr)

options(scipen = 999)

file <- 'c:/Users/jvangeete/Desktop/Asks/big5_model_Tony_rightbackatcha.xlsx'
sheets <- excel_sheets(file)
branded <- read_excel(file, sheets[4])
spotcq <- read_excel(file, sheets[5])
spottrad <- read_excel(file, sheets[6])

attach(branded)
t <- 1:nrow(branded)
t2 <- t*t
lm_branded1 <- lm(Y ~ Impressions + t)
lm_branded2 <- lm(Y ~ Coupons + t)
lm_branded3 <- lm(Y ~ Impressions + Coupons*t + t2) #best model
lm_branded4 <- lm(Y ~ Impressions*Coupons + t)

anova(lm_branded1, lm_branded2, lm_branded3, lm_branded4)
summary(lm_branded3)
detach(branded)

attach(spotcq)
t <- 1:nrow(spotcq)
t2 <- t*t
lm_spotcq1 <- lm(Y ~ Impressions + t)
lm_spotcq2 <- lm(Y ~ Clicks + t)
lm_spotcq3 <- lm(Y ~ Impressions + Clicks*t + t2) #best model
lm_spotcq4 <- lm(Y ~ Impressions*Clicks + t)
lm_spotcq5 <- lm(Y ~ Impressions + t + t2)

anova(lm_spotcq1, lm_spotcq2, lm_spotcq3, lm_spotcq4, lm_spotcq5)
summary(lm_spotcq4)
summary(lm_spotcq5)
detach(spotcq)


attach(spottrad)
t <- 1:nrow(spottrad)
t2 <- t*t
lm_spottrad1 <- lm(Y ~ Impressions + t)
lm_spottrad2 <- lm(Y ~ Clicks + t)
lm_spottrad3 <- lm(Y ~ Impressions + Clicks*t + t2) #best model
lm_spottrad4 <- lm(Y ~ Impressions*Clicks + t)

anova(lm_spottrad1, lm_spottrad2, lm_spottrad3, lm_spottrad4)
summary(lm_spottrad3)
detach(spottrad)
