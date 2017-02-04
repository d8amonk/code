##Spend Estiamtes
options(scipen = 999)
library(dplyr)
library(ggplot2)
library(dummies)
library(RColorBrewer)
require(RGoogleAnalytics)
require(lubridate)
require(RCurl)
require(nzr)

data <- read.csv("C:/Users/aknorr/Documents/Projects/Spend Estimates/Estimate_raw_data.csv", header = T)

data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
brand_dummies <- dummy(data$Brand)
month_dummies <- dummy(data$Month)

data <- cbind(data, brand_dummies, month_dummies)


TW <- filter(data, Brand == "TW")
NT <- filter(data, Brand == "NT")
ST <- filter(data, Brand == "ST")
TF <- filter(data, Brand == "TF")


ggplot(data, aes(x=day30_exp, y = CPA, col= Month))+
  geom_point()+
  geom_smooth(method= "lm")+
  facet_grid(. ~ Brand, scales = "free")+
  scale_color_gradient(low = "yellow", high = "red")


spend_est <- lm(CPA ~ day30_exp + Search_perc + BrandNT + BrandTF + BrandTW + Search_perc*BrandNT + Search_perc*BrandTF + Search_perc*BrandTW + day30_exp*BrandNT + day30_exp*BrandTF + day30_exp*BrandTW + Month2 + Month3 + Month4 + Month5 + Month6 +  Month7 +Month8 + Month9 + Month10 + Month11 + Month12 , data = data)
summary(spend_est)

output <- spend_est$coefficients
output <- as.data.frame(output)
output <- t(output)
row.names(output) <- NULL
output <- as.data.frame(output)

write.csv(output, file = "C:/Users/aknorr/Documents/Projects/Spend Estimates/Reg_output.csv", row.names = FALSE)
ftpUpload(
  "C:/Users/aknorr/Documents/Projects/Spend Estimates/Reg_output.csv",
  "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/Spend_reg_output.csv", userpwd = "dgrabowski:dr0p123"
)







