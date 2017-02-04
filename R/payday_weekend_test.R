library(dplyr)
library(lubridate)
library(ggplot2)

ds <- read.csv("c:/Users/jvangeete/Desktop/weekend_payday_test.csv")

ds$Date <- as.Date(ds$Date)
ds$DOW <- weekdays(ds$Date)
ds$Weekend <- as.integer(ifelse(ds$DOW == 'Saturday' | ds$DOW == 'Sunday', 1, 0 ))
ds$Sales <- ds$SIM + ds$Device

ds$Day_of_month <- day(ds$Date)

ds$Payday3 <- as.integer(ifelse(ds$Day_of_month == 1 | 
                                 ds$Day_of_month == 2 | 
                                 ds$Day_of_month == 3 | 
                                 ds$Day_of_month == 15 | 
                                 ds$Day_of_month == 16 | 
                                 ds$Day_of_month == 17, 1, 0))

ds$Payday2 <- as.integer(ifelse(ds$Day_of_month == 1 | 
                                 ds$Day_of_month == 2 | 
                                 ds$Day_of_month == 15 | 
                                 ds$Day_of_month == 16, 1, 0))

ds$Payday1 <- as.integer(ifelse(ds$Day_of_month == 1 | 
                                 ds$Day_of_month == 15, 1, 0))

# table(ds$DOW)
# table(ds$Weekend)

l1 <- lm(Sales ~ Weekend, ds)
summary(l1)

l2 <- lm(Sales ~ Payday3*as.factor(month(Date)), ds)
summary(l2)

l3 <- lm(Sales ~ Payday2, ds)
summary(l3)

l4 <- lm(Sales ~ Payday1, ds)
summary(l4)

l5 <- lm(Sales ~ Payday3*Weekend, ds)
summary(l5)

ggplot(ds, aes(Weekend, Sales, group = Weekend)) + geom_boxplot() + scale_x_discrete("Weekend?", limits = c(0,1), labels = c("No", "Yes"))+ ylim(c(0,4000))
ggplot(ds, aes(Payday3, Sales, group = Payday3)) + geom_boxplot() + scale_x_discrete("Payday?", limits = c(0,1), labels = c("No", "Yes")) + ylim(c(0,4000)) 
