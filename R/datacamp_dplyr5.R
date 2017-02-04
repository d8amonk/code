library(hflights)
require(dplyr)
glimpse(hflights)

# hflights is in the workspace as a tbl, with translated carrier names

# Make the calculations to end up with ordered statistics per carrier
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(n_flights = n(), 
            n_canc = sum(Cancelled == 1), 
            p_canc = mean(Cancelled == 1) * 100, 
            avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
  arrange(avg_delay, p_canc)

# Answer the question: Which day of the week is average total taxiing time highest?
x <- hflights %>% 
  group_by(DayOfWeek) %>%
  summarise(avg_taxi = mean(TaxiIn + TaxiOut, na.rm=TRUE)) %>%
  arrange(desc(avg_taxi))

plot(x$avg_taxi, type = 'l')

hflights %>% 
  filter(!is.na(ArrDelay)) %>% 
  mutate(p_delay = rank(ArrDelay > 0))

rank <- hflights %>% 
  filter(!is.na(ArrDelay)) %>% 
  group_by(UniqueCarrier) 
mutate(p_delay = mean(ArrDelay > 0))