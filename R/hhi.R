library(dplyr)

hhi <- read.csv("Z:\\DA&DS\\Data Scientists Only\\HHI\\HHI Tracfone SEM_May7_June5_AK_fixed.csv")
x <- hhi$low_50 - sum(hhi$Conversions[hhi$HHI == "11_20" | hhi$HHI == "11_30" | hhi$HHI == "31_40" | hhi$HHI == "41_50"])