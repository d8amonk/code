# dplyr summarise() w length(unique(x)) to id the count of contract
# type in month. count/sum(count) = proportion
# 
# you can also turn a logical test into an aggregating
# function with sum() or mean(). A logical test returns a
# vector of TRUE's and FALSE's. When you apply sum() or 
# mean() to such a vector, R coerces each TRUE to a 1 and 
# each FALSE to a 0. This allows you to find the total number (sum)
# or proportion (mean of 0,1) of observations that passed the test, respectively.
# hflights is available with full names for the carriers

# Be wary of the Dagwood Sandwich problem - your fuctions and args get 
# further and further apart.
summarise(
   mutate(
     filter(
       select(a, X, Y, Z),
       X > Y),
     Q = X + Y + Z)
   all = sum(Q))
# USE CHAINED, PIPED FUNCTIONS (saves space too), pronounce pipe as 'then'
# The %>% operator allows you to extract the first argument of a function from 
# the arguments list and put it in front of it, thus solving the Dagwood sandwich problem.
# Analogues:
mean(c(1, 2, 3, NA), na.rm = TRUE)
c(1, 2, 3, NA) %>% mean(na.rm = TRUE)

p <- hflights  %>% 
  mutate(diff = TaxiOut - TaxiIn)  %>% 
  filter(!is.na(diff)) %>%
  summarise(avg = mean(diff)) 

# eg
hflights$ArrDelay %>% hist( col = 'steel blue', border = 'white', xlim = c(-50,400))

# Sandwich becomes
a %>% 
  select(X, Y, Z) %>% 
  filter(X > Y) %>% 
  mutate (Q = X + Y + Z) %>% 
  summarise(all = sum(Q))

d <- hflights %>%
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>%  
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60)    

# Part 2, concerning flights that had an actual average speed of less then 70 mph.
d %>%
  filter(!is.na(mph), mph <= 70) %>%
  summarise( n_less = n(), 
             n_dest = n_distinct(Dest), 
             min_dist = min(Distance), 
             max_dist = max(Distance))
# Let's define preferable flights as flights that are 150% faster than driving, 
# i.e. that travel 105 mph or greater in real time. Also, assume that cancelled 
# or diverted flights are less preferable than driving.
hflights %>%
  select(Dest, Cancelled, Distance, ActualElapsedTime, Diverted) %>%
  mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>%
  filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>%
  summarise( n_non = n(), 
             p_non = n_non / nrow(hflights) * 100, 
             n_dest = n_distinct(Dest), 
             min_dist = min (Distance), 
             max_dist = max(Distance))

# Calculate the summarizing statistics for flights flown by American Airlines (carrier code "American")
aa <- filter(hflights, 
             UniqueCarrier == "American"
)
s2 <- summarise(aa, 
                n_flights = n(), 
                n_canc = sum(Cancelled == 1), 
                p_canc = mean(Cancelled ==1)*100, 
                avg_delay = mean(ArrDelay, 
                                 na.rm = TRUE)
)