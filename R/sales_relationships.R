##Packages and options

options(scipen = 999)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

##Data clean and manipulation
data <- read.csv("C:/Users/aknorr/Documents/Projects/Sales from Unique imps_cov_perc/raw_data.csv", header = T)
data$DATE <- as.Date(data$DATE, format = "%m/%d/%Y")
data$dow <- weekdays(data$DATE)
data$dow <- factor(data$dow, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
data$milimps <- data$Unique.Imps/1000000
data$spend2 <- data$Total_Spend^2
data$DR2 <- data$DR_Spend^2
data$MID2 <- data$MID_Spend^2


##Date for multi viz
date <- data$DATE 
cov_perc <- data$COV_PERC
total_sales <- data$TOTAL_SALES
dow <- data$dow
cov_perc_pred <- data$cov_per_pred
spend <- data$Total_Spend

d1 <- data.frame(date, cov_perc, dow)
d2 <- data.frame(date, spend, dow)

colnames(d1) <- c("date", "metric", "dow")
colnames(d2) <- c("date", "metric", "dow")

d1$panel <- "COV_PERC"
d2$panel <- "TOTAL_SPEND"

d3 <- rbind(d1, d2)


##Visualizations
ggplot(d3, aes(x= date, y= metric))+
  geom_line()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se =F)+
  facet_grid(panel~dow, scales = "free")

ggplot(data, aes(x= DATE, y= Unique.Imps))+
  geom_line()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se =F)+
  facet_grid(.~dow)
t <- 1:84

ggplot(data, aes(x=DATE, y= COV_PERC))+
  geom_line()+
  geom_bar(data, aes(x=DATE, y= unique.Imps), stat= "identity")

ggplot(data, aes(x=Unique.Imps, y= COV_PERC))+
  geom_point()



##two stage unique imps -> Cov_PErc -> Transactions

stage1 <- lm(COV_PERC ~ milimps +TOTAL_SALES, data = data)
summary(stage1)

data$cov_per_pred <- predict(stage1)

ggplot(data, aes(x= DATE, y= cov_per_pred))+
  geom_line()

stage2 <- lm(TRANSACTIONS ~ cov_per_pred +TOTAL_SALES, data = data)
summary(stage2)

##How spend affects unique impressiosn

total_s <- lm(U_Imps ~ Total_Spend + spend2 +t, data = data)
summary(total_s)

funnel_s <- lm(U_Imps ~ DR_Spend + MID_Spend + DR2 + MID2 + t, data = data)
summary(funnel_s)

ggplot(data, aes(x=DATE, y=MID_Spend))+
  geom_line()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se =F)


