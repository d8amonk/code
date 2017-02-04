require(ggplot2) 
require(maps)
require(ggthemes) #Biobase
require(dplyr)

targets <- read.csv("Z:/DA&DS/Data Scientists Only/Walmart Data/Target_Stores_Historical.csv")
glimpse(targets)

targets$Open.Date <- targets$Open.Date %>% as.Date(format = '%m/%d/%Y')
targets$City <- targets$City %>% as.character()
targets$State <- targets$State %>% as.character()

targets <- rename(targets, Store = Store.., Year = Fiscal.Open.Year)

targets_state_yr_grp <- targets %>% 
  group_by(State, Year) %>% 
  mutate(Stores = n()) %>% 
  select(Year, State, Target.Stores = Stores) %>% 
  distinct()
  
targets_yr_grp <- targets %>% 
  group_by(Year) %>% 
  mutate(Stores = n()) %>% 
  select(Year, Target.Stores = Stores) %>% 
  distinct()
  
targets_state_grp <- targets %>% 
  group_by(State) %>% 
  mutate(Stores = n()) %>% 
  select(State, Target.Stores = Stores) %>% 
  distinct()
  
View(targets_state_yr_grp)
View(targets_yr_grp)
View(targets_state_grp)

ggplot(targets_state_grp)







source("http://copy.com/zEtAXJC8tG7yv7Zz")
head(state.key)


  
  