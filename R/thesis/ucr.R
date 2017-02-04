setwd("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/UCR/Cleaned/v4")
library(dplyr)
# change the file name in the top and bottom to pivot by_city data to state_year
c <- read.csv("06tbl08_v4.csv")

c$State <- as.character(c$State)

library(dplyr)
d <- c %>% 
  group_by(State) %>% 
  mutate(Population = sum(Population),
         Violent.Crime = sum(Violent.Crime), 
         Murder = sum(Murder), 
         Rape = sum(Rape),
         Robbery = sum(Robbery),
         Agg.Assault = sum(Agg.Assault), 
         Property.Crime = sum(Property.Crime), 
         Burglary = sum(Burglary), 
         Larceny.Theft = sum(Larceny.Theft), 
         Motor.Vehicle.Theft = sum(Motor.Vehicle.Theft), 
         Arson = sum(Arson)) %>% 
  distinct() %>% select(State, Year, Population, Violent.Crime, Murder, Rape, Robbery,
                        Agg.Assault, Property.Crime, Burglary, Larceny.Theft, Motor.Vehicle.Theft, Arson)

write.csv(d, "06tbl08_v4.csv")

# rowbind them all together.
rm(list = ls())

oh6 <- read.csv("06tbl08_v4.csv")
oh7 <- read.csv("07tbl08_v4.csv")
oh8 <- read.csv("08tbl08_v4.csv")
oh9 <- read.csv("09tbl08_v4.csv")
oh10 <- read.csv("10tbl08_v4.csv")
oh11 <- read.csv("11tbl08_v4.csv")
oh12 <- read.csv("12tbl08_v4.csv")
oh13 <- read.csv("13tbl08_v4.csv") #only 50 obs = HI drops off?

a <- rbind(oh6,oh7,oh8,oh9,oh10,oh11,oh12,oh13)

write.csv(a, "allStates_crime.csv") 

rm(list = ls())

# bring back in the ucr
allStates_crime <- read.csv("allStates_crime.csv")
allStates_crime$ID <- as.character(allStates_crime$ID)
allStates_crime$State <- as.character(allStates_crime$State)

setwd("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/Merges/")
unemp <- read.csv("old/allStates_unemp.csv")
unemp$ID <- as.character(unemp$ID)
unemp$State <- as.character(unemp$State)

# make sure dplyr is loaded, join time
allStates_crime_unemp <- left_join(allStates_crime, unemp, by = "ID")
anti_join(allStates_crime, unemp) #great QA function; should be == 0

write.csv(allStates_crime_unemp, "crime_unemp_v2.csv")
rm(list = ls())

gs <- read.csv("gear_spend_v1.csv")
cu <- read.csv("crime_unemp_v2.csv")

# gs <- rbind(gs, matrix(NA, 13, 4, dimnames = list(c(), colnames(gs))))
# cu <- rbind(cu, matrix(NA, 52, 17, dimnames = list(c(), colnames(cu))))
cug <- left_join(gs, cu)

# paste(cug$State.x, cug$Year.x) == paste(cug$State.y, cug$Year.y) #great QA #NAHT = this used to be a join; the QA doesn't catch dupes!
# paste(cug$State.y, cug$Year.y) == paste(cug$State, cug$Year) #great QA
write.csv(cug, "allStates_crime_unemp_gear.csv")
# go put the 2014 gear spend data into the cug.csv manually and fill with NA

rm(list = ls())
cug <- read.csv("allStates_crime_unemp_gear.csv")
acs <- read.csv("stata_slim.csv")

full_set <- left_join(cug, acs)
write.csv(full_set, "fullset.csv")

full_set <- read.csv("fullset.csv")

library(stargazer)
stargazer(full_set)
# deal with this in console!

ggplot(full_set, aes(State, Gear_Spend)) + geom_histogram(stat = "identity") + theme_calc()


