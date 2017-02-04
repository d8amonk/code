test_daily <- read.csv("C:/Users/aknorr/Documents/R/Test_all_daily_2.csv", header = T)
 
test_gm <- read.csv("C:/Users/aknorr/Documents/R/Test_all_GM.csv", header = T)

library(ggplot2)
library(dplyr)
library(GGally)
library(forecast)

##GM transforms

test_gm$Date <- as.Date(test_gm$Date, format = "%m/%d/%Y")

test_gm$GM_percent <- as.numeric(test_gm$GM.)

test_gm$GM_percent <- test_gm$GM_percent/100

test_gm$GM. <- NULL

summary(test_gm)
head(test_gm)

##Daily transforms

test_daily$Date <- as.Date(test_daily$Date, format = "%m/%d/%Y")

summary(test_daily)
head(test_daily)

##Combine on Date

Combined <- merge(test_gm, test_daily)
Combined$log_sales <- log(Combined$Sales)
Combined$log_cost <- log(Combined$Cost)
Combined$log_rev <- log(Combined$Revenue)

summary(Combined)

##Log sales, Cost, and Impressions
ggpairs(Combined[, c("log_sales", "Cost", "IMPRESSIONS", "CLICKS", "BILLABLE_QUANTITY")])
##Log Revenue, Cost, Impressions
ggpairs(Combined[, c("log_rev", "Cost", "IMPRESSIONS", "CLICKS", "BILLABLE_QUANTITY")])
##Cost, Impressions, Clicks
ggpairs(Combined[, c("Total.Cost", "IMPRESSIONS", "CLICKS")])
##Bill Quantity, Impressions, Clicks
ggpairs(Combined[, c("BILLABLE_QUANTITY", "IMPRESSIONS", "CLICKS")])
##Sales on Clicks from each group
ggpairs(Combined[, c("log_sales", "SEM.Clicks", "CPA.Clicks", "Mid.Funnel.Display.Clicks", "Mid.Funnel.SEM.Clicks")])

##Cost pairs on rev
ggpairs(Combined_drop_outlier[ ,c("log_rev", "SEM.Cost", "CPA.Cost", "Mid.Funnel.Cost")])

##Possible outlier in Mid.Funnel.Display.Clicks? 

Combined_drop_outlier <- filter(Combined, Mid.Funnel.Display.Clicks <= 150000)
ggpairs(Combined_drop_outlier[, c("log_sales", "SEM.Clicks", "CPA.Clicks", "Mid.Funnel.Display.Clicks", "Mid.Funnel.SEM.Clicks")])

##Some LM model tests

clicks_drive <- lm(log_sales ~ SEM.Clicks + CPA.Clicks + Mid.Funnel.Display.Clicks + Mid.Funnel.SEM.Clicks, data = Combined)
summary(clicks_drive)

clicks_drive$coefficients*100


clicks_drive_drop <- lm(log_sales ~ SEM.Clicks + CPA.Clicks + Mid.Funnel.Display.Clicks + Mid.Funnel.SEM.Clicks, data = Combined_drop_outlier)
summary(clicks_drive_drop)

clicks_drive_drop$coefficients*100

rev_cost_drive <- lm(log_rev ~ SEM.Cost + CPA.Cost + Mid.Funnel.Cost, data = Combined_drop_outlier)
summary(rev_cost_drive)



##PREDICTING SALES BASED ON CLICKS ACROSS THE FUNNEL

ggplot(Combined_drop_outlier, aes(x=Date, y=Sales))+
  geom_line()

Combined_drop_outlier$sales_adj <- seasadj(Combined_drop_outlier$Sales)

tsdisplay(diff(Combined_drop_outlier$Sales), main="")
tsdisplay(Combined_drop_outlier$Sales, main="")


arima_test_auto <- auto.arima(Combined_drop_outlier$Sales, xreg= Combined_drop_outlier[, c("SEM.Clicks", "CPA.Clicks", "Mid.Funnel.Display.Clicks","Mid.Funnel.SEM.Clicks")])

arima_test <- arima(Combined_drop_outlier$Sales, order = c(1,1,0), xreg= Combined_drop_outlier[, c("SEM.Clicks", "CPA.Clicks", "Mid.Funnel.Display.Clicks","Mid.Funnel.SEM.Clicks")])

summary(arima_test_auto)
summary(arima_test)

Acf(residuals(arima_test_auto))
Acf(residuals(arima_test))

Box.test(residuals(arima_test_auto), type="Ljung")
Box.test(residuals(arima_test), type="Ljung")

fcast <- forecast(arima_test_auto, h=30, xreg= Combined_drop_outlier[, c("SEM.Clicks", "CPA.Clicks", "Mid.Funnel.Display.Clicks","Mid.Funnel.SEM.Clicks")])
fcast_2 <- forecast(arima_test, h=30, xreg= Combined_drop_outlier[, c("SEM.Clicks", "CPA.Clicks", "Mid.Funnel.Display.Clicks","Mid.Funnel.SEM.Clicks")])

plot(fcast)
plot(fcast_2)

summary(fcast)
summary(fcast_2)


ggplot(Combined_drop_outlier, aes(x=Date, y=diff(CPA.Clicks, 1)))+
  geom_line()
