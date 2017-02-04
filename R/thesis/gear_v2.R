# #we need:
# unemployment (BLS)
# per capita income (BLS, ACS)
# measures of race, age hhi, hhv, pop, %black, %hispan, seg.by.age, 
# 
# (violent, property) crime levels, report/arrests
# 
# think lags
# 
# unit of ob: stateyear,
# 
# dep var: $val_gear

library(dplyr)

gear_06 <- read.csv(file.choose())
gear_94 <- read.csv(file.choose())
glimpse(gear_06)
glimpse(gear_94)
summary(gear_06)
summary(gear_94)

# FORMAT A LA CARTE!
gear_06$State  <- gear_06$State %>% as.character()
gear_06$State_Ab  <- gear_06$State_Ab %>% as.character()
# gear_06$Agency.Name <- gear$Agency.Name %>% as.character()
# gear_06$County  <- gear$County %>% as.character()
# gear_06$Item.Name  <- gear$Item.Name %>% as.character()
# gear_06$UI  <- gear$UI %>% as.character()
gear_06$Date  <- gear_06$Date %>% as.Date(format = "%M/%D/%Y")
# gear_06$Date  <- gear$Date %>% as.character()

# FORMAT A LA CARTE!
gear_94$State  <- gear$State %>% as.character()
gear_94$Agency.Name <- gear$Agency.Name %>% as.character()
gear_94$County  <- gear$County %>% as.character()
gear_94$Item.Name  <- gear$Item.Name %>% as.character()
gear_94$UI  <- gear$UI %>% as.character()
gear_94$Date  <- gear$Date %>% as.Date(format = "%m/%D/%Y")
gear_94$Date  <- gear$Date %>% as.character()

summary(gear)

# gear$Ship.Date <- as.POSIXct(gear$Ship.Date, formet = "%m/%D/%Y")

gear06_stateyear <- 
  gear_06 %>% 
  group_by(State, Year) %>% 
  mutate(Total.Cost = as.numeric(sum(Acquisition.Cost)),
         Total.Quantity = as.integer(sum(Quantity))) 

gear06_distinct <- distinct(gear06_stateyear)

gear94_byCounty <- 
  gear %>% 
  group_by(State, County, Year, Item.Name) %>% 
  mutate(Total.Cost = as.numeric(sum(Acquisition.Cost)),
         Total.Quantity = as.integer(sum(Quantity))) 

gear94_distinct <- distinct(gear_byCounty)

# gear_distinct$Month <- as.numeric(levels(gear_distinct$Year))[gear_distinct$Year]
# gear_distinct$Year <- as.numeric(as.character(gear_distinct$Year))
# sort(gear_distinct, gear_distinct$Year)#, gear_distinct$Month)

library(ggplot2)
g <- ggplot(gear_distinct, aes(x = Total.Cost))
g + geom_density(aes(y = ..density..), xlim = c(0,10000000)) #not working
# todo 9.13: trim x-axis

table(gear_94$State, gear_94$Year)

xtabs(~State+Year)

library(reshape)
cast(gear, State ~ Year, fun = mean) #doesn't really work yet

# fill cell values with first var in a table of listed vars  
t1 <- tapply(count(gear$County),list(gear$State,gear$Year),mean)

# take the __f()___ of __obj___
mean(tapply(Acquisition.Cost,list(gear$State,gear$Year),mean), na.rm = T)
