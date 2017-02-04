library(readxl)
library(dplyr)

options(scipen = 999)

file <- 'c:/Users/jvangeete/Desktop/Asks/Copy of big5_model_Tony_rightbackatcha.xlsx'
sheets <- excel_sheets(file)
branded <- read_excel(file, sheets[5])
spotcq <- read_excel(file, sheets[6])
spottrad <- read_excel(file, sheets[7])

t <- 1:nrow(branded)
t2 <- t*t
lm_branded1 <- lm(Y ~ Impressions + t, data = branded)
lm_branded2 <- lm(Y ~ Coupons + t, data = branded)
lm_branded3 <- lm(Y ~ Impressions + Coupons*t + t2, data = branded) #best model
lm_branded4 <- lm(Y ~ Impressions*Coupons + t, data = branded)

anova(lm_branded1, lm_branded2, lm_branded3, lm_branded4)
summary(lm_branded3)

t <- 1:nrow(spotcq)
t2 <- t*t
lm_spotcq1 <- lm(Y ~ Impressions + t, data = spotcq)
lm_spotcq2 <- lm(Y ~ Clicks + t, data = spotcq)
lm_spotcq3 <- lm(Y ~ Impressions + Clicks*t + t2, data = spotcq) #best model
lm_spotcq4 <- lm(Y ~ Impressions*Clicks + t, data = spotcq)
lm_spotcq5 <- lm(Y ~ Impressions + t + t2, data = spotcq)

anova(lm_spotcq1, lm_spotcq2, lm_spotcq3, lm_spotcq4, lm_spotcq5)
summary(lm_spotcq3)
summary(lm_spotcq5)


t <- 1:nrow(spottrad)
t2 <- t*t
lm_spottrad1 <- lm(Y ~ Impressions + t, data = spottrad)
lm_spottrad2 <- lm(Y ~ Clicks + t, data = spottrad)
lm_spottrad3 <- lm(Y ~ Impressions + Clicks*t + t2, data = spottrad) #best model
lm_spottrad4 <- lm(Y ~ Impressions*Clicks + t, data = spottrad)

anova(lm_spottrad1, lm_spottrad2, lm_spottrad3, lm_spottrad4)
summary(lm_spottrad3)

ggplot(branded, aes(x = Date, y = Impressions, label = "Impressions")) + 
  geom_line(col = 'red', lwd = 1.1) +
  geom_line(aes(y = Clicks, label = "Clicks"), col = 'blue', lwd = 1.1)

# ggplot eg
d=data.frame(time=c(1,2,5,6,8,2), space=c(3,6,2,8,7,5), suite=c('spec','spec','dacapo','dacapo','dacapo','spec'), bm=c('javac','db', 'antlr', 'bloat', 'fop', 'raytrace'))
ex=subset(d, time<=min(time)|time>=max(time)|space<=min(space)|space>=max(space))
ggplot() + 
scale_y_continuous(limits=c(1.5, 8)) +
geom_point(data=d, mapping=aes(x=time, y=space, fill=suite), size=9, shape=21, color="black") +
geom_text(data=d, mapping=aes(x=time, y=space, label=substr(suite, 1, 1)), size=6) + 
geom_text(data=ex, mapping=aes(x=time, y=space, label=bm), size=4, vjust=3, hjust=0.5) +
opts(title="geom_text", plot.title=theme_text(size=40, vjust=1.5))
