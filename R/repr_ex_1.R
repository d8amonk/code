library(ggplot2)
library(TTR)

set.seed(1984)

start <- as.Date('2016-01-01')
end <- Sys.Date()

days <- as.numeric(end - start)

cost <- rnorm(days, mean = 45400, sd = 11640)
date <- seq.Date(from = start, to = end - 1, by = 'day') 
cost_7 <- SMA(cost, 7)
cost_30 <- SMA(cost, 30)

df <- data.frame(Date = date, Cost = cost, Cost_7 = cost_7, Cost_30 = cost_30)

left <- end - 31
right <- end - 1

k <- 1000

# plot series
ggplot(df, aes(x = Date, y = Cost/k))+
  geom_line(lwd = 0.5) +
  geom_line(aes(y = Cost_7), col = 'red', linetype = 3, lwd = 1) +
  geom_line(aes(y = Cost_30), col = 'blue', linetype = 5, lwd = 0.75) +
  xlim(c(left, right)) + 
  ylim(c(min(df$Cost[df$Date > left]), max(df$Cost[df$Date > left]))/k) +
  xlab("")