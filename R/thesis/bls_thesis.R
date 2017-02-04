gear <- read.csv(file.choose())
ce <- read.csv(file.choose())

setwd("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/Merges")
library(dplyr)
ceg <- merge(ce, gear, by = "ID")
ceg_2 <- dplyr::left_join(gear, ce, by = "ID")

write.csv(ceg_2, "ceg_2.csv")

ceg_3 <- read.csv(file.choose())
attach(ceg_3)
l1 <- lm(Gear_Spend ~ Avg_unemp)
l2 <- lm(Gear_Spend ~ Avg_unemp + VC + PC)
l3 <- lm(VC ~ Avg_unemp)
l4 <- lm(PC ~ Avg_unemp)

