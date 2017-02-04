# C:\Users\jvangeete\Google Drive\School\UCD\Thesis\datasets\UCR\Cleaned
# as.numeric(levels(f))[f]
library(dplyr)

#load the data
crime_06 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/06tbl08.csv")
crime_07 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/07tbl08.csv")
crime_08 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/08tbl08.csv")
crime_09 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/09tbl08.csv")
crime_10 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/10tbl08.csv")
crime_11 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/11tbl08.csv")
crime_12 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/12tbl08.csv")
crime_13 <- read.csv("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/13tbl08.csv")

crime_06 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/06tbl08.csv")
crime_07 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/07tbl08.csv")
crime_08 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/08tbl08.csv")
crime_09 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/09tbl08.csv")
crime_10 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/10tbl08.csv")
crime_11 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/11tbl08.csv")
crime_12 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/12tbl08.csv")
crime_13 <- read.csv("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v2/13tbl08.csv")


glimpse(crime_06)
summary(crime_06)

# crime_06 <- crime_06[!is.na(crime_06)]

# crime_06$Year <- as.character(crime_06$Year)

crime_06$ID <- as.character(crime_06$ID)
crime_06$State <- as.character(crime_06$State)
crime_06$City <- as.character(crime_06$City)

crime_07$ID <- as.character(crime_07$ID)
crime_07$State <- as.character(crime_07$State)
crime_07$City <- as.character(crime_07$City)

crime_08$ID <- as.character(crime_08$ID)
crime_08$State <- as.character(crime_08$State)
crime_08$City <- as.character(crime_08$City)

crime_09$ID <- as.character(crime_09$ID)
crime_09$State <- as.character(crime_09$State)
crime_09$City <- as.character(crime_09$City)

crime_10$ID <- as.character(crime_10$ID)
crime_10$State <- as.character(crime_10$State)
crime_10$City <- as.character(crime_10$City)

crime_11$ID <- as.character(crime_11$ID)
crime_11$State <- as.character(crime_11$State)
crime_11$City <- as.character(crime_11$City)

crime_12$ID <- as.character(crime_12$ID)
crime_12$State <- as.character(crime_12$State)
crime_12$City <- as.character(crime_12$City)

crime_13$ID <- as.character(crime_13$ID)
crime_13$State <- as.character(crime_13$State)
crime_13$City <- as.character(crime_13$City)

# crime_06$Population <- as.numeric(sub(",","",crime_06$Population))
# crime_06$VC <- as.numeric(sub(",","",crime_06$VC))
# crime_06$MANM <- as.numeric(sub(",","",crime_06$MANM))
# crime_06$FR <- as.numeric(sub(",","",crime_06$FR))
# crime_06$Robbery <- as.numeric(sub(",","",crime_06$Robbery))
# crime_06$AA <- as.numeric(sub(",","",crime_06$AA))
# crime_06$PC <- as.numeric(sub(",","",crime_06$PC))
# crime_06$Burglary <- as.numeric(sub(",","",crime_06$Burglary))
# crime_06$LT <- as.numeric(sub(",","",crime_06$LT))
# crime_06$MVT <- as.numeric(sub(",","",crime_06$MVT))
# crime_06$Arson <- as.numeric(sub(",","",crime_06$Arson))

#groupby state_year and sum crime valunemps
ucr06 <- crime_06 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr07 <- crime_07 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr08 <- crime_08 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr09 <- crime_09 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr10 <- crime_10 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr11 <- crime_11 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr12 <- crime_12 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)
ucr13 <- crime_13 %>% 
  group_by(State, Year) %>% 
  mutate(Total.pop = sum(Population),
         Total.VC = sum(VC), 
         Total.MANM = sum(MANM), 
         Total.FR = sum(FR), 
         Total.Robbery = sum(Robbery), 
         Total.AA = sum(AA), 
         Total.PC = sum(PC), 
         Total.Burglary = sum(Burglary), 
         Total.LT = sum(LT), 
         Total.MVT = sum(MVT), 
         Total.Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Total.pop, Total.VC, Total.MANM, Total.FR, Total.Robbery,
                        Total.AA, Total.PC, Total.Burglary, Total.LT, Total.MVT, Total.Arson)

colnames(ucr06) == colnames(ucr13) #check colnames before rbind

# bind all the pivoted years together
ucr_0613 <- rbind(ucr06,ucr07,ucr08,ucr09,ucr10,ucr11,ucr12,ucr13)
any(is.na(ucr_0613)) #check to make sure everything made it; troubleshoot:
# which(is.na(ucr_0613)) #check to make sure everything made it
write.csv(ucr_0613, "ucr_0613_merged.csv")
# go gen the ID = concat(state,year) var

# rm(list = ls())

# bind together crime and unemployment data
ucr_0613 <- read.csv("ucr_0613_merged.csv")
unemp <- read.csv("all_States_unemp.csv")
crime_unemp <- left_join(ucr_0613, unemp, "ID")
write.csv(crime_unemp,"crime_unemp.csv")
# go gen the ID = concat(state,year) var

# bind together the crime+unemp with the gear sales data
crime_unemp <- read.csv("crime_unemp.csv")
gear <- read.csv("gear_spend_v1.csv")

crime_unemp_gear <- left_join(crime_unemp, gear, "ID")
write.csv(crime_unemp_gear,"crime_unemp_gear.csv")

setwd("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/Merges")

crime_unemp_gear <- read.csv("crime_unemp_gear.csv")

library(stargazer)

stargazer(crime_unemp_gear, digits = 2, notes = "Not all variables present.", notes.align = "c")



