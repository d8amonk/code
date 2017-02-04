# Quandl crime
library(Quandl)
Quandl.auth("fNEwUL5rC28Sz23q6NGD")
burglaries <- Quandl("FBI_UCR/USCRIME_TYPE_ROBBERIES", authcode="fNEwUL5rC28Sz23q6NGD", trim_start="2000-01-01")
plot(burglaries$Year, burglaries$Utah, type = 'l')

gear <- read.csv(file.choose())
str(gear)

View(gear)

gear$Ship.Date <- as.POSIXct(gear$Ship.Date, formet = "%m/%D/%Y")

library(dplyr)
gear_byCounty <- 
  gear %>% 
  group_by(County, Year, Month, Item.Name) %>% 
  mutate(Total.Cost = as.numeric(sum(Acquisition.Cost)),
         Total.Quantity = as.integer(sum(Quantity))) 

gear_distinct <- distinct(gear_byCounty)

gear_distinct$Month <- as.numeric(levels(gear_distinct$Year))[gear_distinct$Year]
gear_distinct$Year <- as.numeric(as.character(gear_distinct$Year))
sort(gear_distinct, gear_distinct$Year)#, gear_distinct$Month)

library(ggplot2)
g <- ggplot(gear_distinct, aes(x = Ship.Date, y = Total.Cost))
g + geom_scatter()
