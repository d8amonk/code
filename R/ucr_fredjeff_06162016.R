require(dplyr)

rm(list = ls())
data_UR <- read.csv("\\\\fileserver/company/DA&DS/Data Scientists Only/MEDIA TAKEOVER/Tracfone Modelling Data Redone.csv")

# data_UR$DATE

data_UR$DATE <- as.Date(data_UR$DATE, format = "%m/%d/%Y")

# sum(is.na(data_UR))
# 
# sum(is.na(data_UR[1:ncol(data_UR)]))
# 
# na_count <-sapply(data_UR, function(y) sum(length(which(is.na(y)))))

data_UR$Coverage_Percentage <- data_UR$Coverage_Percentage*100 

summary(lm1 <- lm(data_UR$Coverage_Percentage ~ 
            0 +
            data_UR$Total_Transactions +
            data_UR$eBay_U +
            data_UR$TTD_U +
            data_UR$Facebook_U +
            data_UR$Simplifi_U +
            data_UR$Adiant_U +
            data_UR$GDN_U +
            data_UR$YouTube_U +
            data_UR$Quantcast_U +
            data_UR$Juice.Mobile_U))
summary(lm2 <- lm(data_UR$Coverage_Percentage ~ 
            0 +
            data_UR$Total_Transactions))

summary(lm3 <- lm(data_UR$Coverage_Percentage ~ 
                    0 +
                    log(data_UR$Total_Transactions) +
                    data_UR$eBay_U))

