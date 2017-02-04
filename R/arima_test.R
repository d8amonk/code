##Data load

Combined_all <- read.csv("C:/Users/aknorr/Documents/R/GM_Send_Combined_01182016.csv", header = T)

##Packages
library(ggplot2)
library(dplyr)
library(GGally)
library(forecast)

## Correct Date

Combined_all$Date <- as.Date(Combined_all$Date, format = "%m/%d/%Y")

##adding variables
Combined_all$act_sales_log <- log(Combined_all$Actualized.Sales)

##crazy excel nulls removed
Combined_all <- na.omit(Combined_all)

summary(Combined_all)

ggplot(Combined_all, aes(x=Date, y=act_sales_log))+
  geom_line()

tsdisplay(Combined_all$act_sales_log, main = "")
tsdisplay(diff(Combined_all$act_sales_log), main = "")

test_arima_auto <- auto.arima(Combined_all$act_sales_log, d = 1)

summary(test_arima_auto)

Acf(residuals(test_arima_auto))

Box.test(residuals(test_arima_auto), lag = 10, fitdf = 0,  type="Ljung") ##this is wrong. Need to set fitdf= and lag= UNSURE RIGHT NOW

fcast <- forecast(test_arima_auto, h = 100)

plot(fcast)

summary(fcast)


duh_lm<- lm(act_sales_log ~ Total.Cost + Total.Sales + Total.Clicks + Total.IMP, data = Combined_all)
summary(duh_lm)

test_lm <- lm(act_sales_log ~ SEM.Gross.Adds + CPA.Gross.Adds + Mid.Gross.Adds, data = Combined_all)
summary(test_lm)

test_device_sim_lm <- lm(act_sales_log ~ SEM.Device.Sales + SEM.SIM.Sales + CPA.Device.Sales + CPA.SIM.Sales + Mid.Funnel.Device.Sales + Mid.Funnel.SIM.Sales, 
                         data = Combined_all)
summary(test_device_sim_lm)