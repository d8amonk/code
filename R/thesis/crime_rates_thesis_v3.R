library(dplyr)
library(lmtest)
library(stargazer)
library(plm)

setwd("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/Merges/")
full_set <- read.csv("fullset.csv", stringsAsFactors = FALSE)
fullset_WC <- read.csv("fullset_IS.csv", stringsAsFactors = FALSE)
fullset_noWC <- read.csv("fullset_noWC.csv", stringsAsFactors = FALSE)

full_set[full_set == 0] <- 1
fullset_WC[fullset_WC == 0] <- 1
fullset_noWC[fullset_noWC == 0] <- 1

# double book, routing
fs <- full_set
WC <- fullset_WC
noWC <- fullset_noWC

# make sure you're using the agg-summed vc and pc; +1 for log adj above
all(fs$Violent.Crime == fs$Murder + fs$Rape + fs$Robbery + fs$Agg.Assault | 
      fs$Murder + fs$Rape + fs$Robbery + fs$Agg.Assault + 1, na.rm = TRUE)
all(WC$Violent.Crime == WC$Murder + WC$Rape + WC$Robbery + WC$Agg.Assault | 
      WC$Murder + WC$Rape + WC$Robbery + WC$Agg.Assault + 1, na.rm = TRUE)
all(noWC$Violent.Crime == noWC$Murder + noWC$Rape + noWC$Robbery + noWC$Agg.Assault | 
      noWC$Murder + noWC$Rape + noWC$Robbery + noWC$Agg.Assault + 1, na.rm = TRUE)
# ensure all true

fs <- fs %>% 
  mutate(
    vc_rate = as.numeric(Violent.Crime / (Population/10000)),
    pc_rate = as.numeric(Property.Crime / (Population/10000)),
    murder_rate = as.numeric(Murder / (Population/10000)),
    rape_rate = as.numeric(Rape / (Population/10000)),
    rob_rate = as.numeric(Robbery / (Population/10000)),
    assault_rate = as.numeric(Agg.Assault / (Population/10000)),
    burg_rate = as.numeric(Burglary / (Population/10000)),
    larc_rate = as.numeric(Larceny.Theft / (Population/10000)),
    mvt_rate = as.numeric(Motor.Vehicle.Theft / (Population/10000)),
    arson_rate = as.numeric(Arson / (Population/10000))
  )

WC <- WC %>% 
  mutate(
    vc_rate = as.numeric(Violent.Crime / (Population/10000)),
    pc_rate = as.numeric(Property.Crime / (Population/10000)),
    murder_rate = as.numeric(Murder / (Population/10000)),
    rape_rate = as.numeric(Rape / (Population/10000)),
    rob_rate = as.numeric(Robbery / (Population/10000)),
    assault_rate = as.numeric(Agg.Assault / (Population/10000)),
    burg_rate = as.numeric(Burglary / (Population/10000)),
    larc_rate = as.numeric(Larceny.Theft / (Population/10000)),
    mvt_rate = as.numeric(Motor.Vehicle.Theft / (Population/10000)),
    arson_rate = as.numeric(Arson / (Population/10000))
  )

noWC <- noWC %>% 
  mutate(
    vc_rate = as.numeric(Violent.Crime / (Population/10000)),
    pc_rate = as.numeric(Property.Crime / (Population/10000)),
    murder_rate = as.numeric(Murder / (Population/10000)),
    rape_rate = as.numeric(Rape / (Population/10000)),
    rob_rate = as.numeric(Robbery / (Population/10000)),
    assault_rate = as.numeric(Agg.Assault / (Population/10000)),
    burg_rate = as.numeric(Burglary / (Population/10000)),
    larc_rate = as.numeric(Larceny.Theft / (Population/10000)),
    mvt_rate = as.numeric(Motor.Vehicle.Theft / (Population/10000)),
    arson_rate = as.numeric(Arson / (Population/10000))
  )

# full set of states, 2x FE Model with Linear Time Trend
plm.fs <- plm(log(Gear_Spend) ~ murder_rate + 
               rape_rate +
               rob_rate +
               assault_rate +
               burg_rate +
               larc_rate +
               mvt_rate +
               arson_rate +
               Hispanic +
               Black +
               NativeIndian +
               Asian + 
               Islander +
               OtherRace +
               log(Population), fs[complete.cases(fs),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(plm.fs)
plm.fs_l <- plm(log(Gear_Spend) ~ murder_rate + 
               rape_rate +
               rob_rate +
               assault_rate +
               burg_rate +
               larc_rate +
               mvt_rate +
               arson_rate +
               Hispanic +
               Black +
               NativeIndian +
               Asian + 
               Islander +
               OtherRace + 
               Year +
               log(Population), fs[complete.cases(fs),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(plm.fs_l)

stargazer(plm.fs, plm.fs_l)

plm.fs <- coeftest(plm.fs, vcov=vcovHC)
plm.fs

# west coast states only
plm.WC <- plm(log(Gear_Spend) ~ murder_rate + 
               rape_rate +
               rob_rate +
               assault_rate +
               burg_rate +
               larc_rate +
               mvt_rate +
               arson_rate +
               Hispanic +
               Black +
               NativeIndian +
               Asian + 
               Islander +
               OtherRace + 
               log(Population), WC[complete.cases(WC),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(plm.WC)
plm.WC <- coeftest(plm.WC, vcov=vcovHC)
plm.WC

# no west coast states
plm.noWC <- plm(log(Gear_Spend) ~ murder_rate + 
               rape_rate +
               rob_rate +
               assault_rate +
               burg_rate +
               larc_rate +
               mvt_rate +
               arson_rate +
               Hispanic +
               Black +
               NativeIndian +
               Asian + 
               Islander +
               OtherRace + 
               log(Population), noWC[complete.cases(noWC),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(plm.noWC)
plm.noWC <- coeftest(plm.noWC, vcov=vcovHC)
plm.noWC

plm.lag1 <- plm(log(Gear_Spend) ~ lag(murder_rate,1) + 
               lag(rape_rate,1) +
               lag(rob_rate,1) +
               lag(assault_rate,1) +
               lag(burg_rate,1) +
               lag(larc_rate,1) +
               lag(mvt_rate,1) +
               lag(arson_rate,1) +
               lag(Hispanic,1) +
               lag(Black,1) +
               lag(NativeIndian,1) +
               lag(Asian,1) + 
               lag(Islander,1) +
               lag(OtherRace,1) + 
               log(Population), fs[complete.cases(fs),], 
               index=c("State", "Year"), 
               model= "within", 
               effect = "twoway")
summary(plm.lag1)
plm.lag1 <- coeftest(plm.lag1, vcov=vcovHC)
plm.lag1

plm.lag2 <- plm(log(Gear_Spend) ~ lag(murder_rate,2) + 
               lag(rape_rate,2) +
               lag(rob_rate,2) +
               lag(assault_rate,2) +
               lag(burg_rate,2) +
               lag(larc_rate,2) +
               lag(mvt_rate,2) +
               lag(arson_rate,2) +
               lag(Hispanic,2) +
               lag(Black,2) +
               lag(NativeIndian,2) +
               lag(Asian,2) + 
               lag(Islander,2) +
               lag(OtherRace,2) + 
               log(Population), fs[complete.cases(fs),], 
               index=c("State", "Year"), 
               model= "within", 
               effect = "twoway")
summary(plm.lag2)
plm.lag2 <- coeftest(plm.lag2, vcov=vcovHC)
plm.lag2

plm.lag3 <- plm(log(Gear_Spend) ~ lag(murder_rate,3) + 
               lag(rape_rate,3) +
               lag(rob_rate,3) +
               lag(assault_rate,3) +
               lag(burg_rate,3) +
               lag(larc_rate,3) +
               lag(mvt_rate,3) +
               lag(arson_rate,3) +
               lag(Hispanic,3) +
               lag(Black,3) +
               lag(NativeIndian,3) +
               lag(Asian,3) + 
               lag(Islander,3) +
               lag(OtherRace,3) + 
               log(Population), fs[complete.cases(fs),], 
               index=c("State", "Year"), 
               model= "within", 
               effect = "twoway")
summary(plm.lag3)
plm.lag3 <- coeftest(plm.lag3, vcov=vcovHC)
plm.lag3


plm.race <- plm(log(Gear_Spend) ~ 
               White +
               Hispanic +
               Black +
               NativeIndian +
               Asian + 
               Islander +
               OtherRace + 
               OutOfState +
               log(Population), WC[complete.cases(WC),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(plm.race)
plm.race <- coeftest(plm.race, vcov=vcovHC)
plm.race

stargazer(plm.fs, plm.WC, plm.noWC, title="Aggregate Crime Rates; Race, 2-Way FEs, CSEs")