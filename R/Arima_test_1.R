u <- c(16.5,17.45,18.4,19.35,20.3,20.72)
cci <- c(14, 16, 12, 7, 9, 10)
bci <- c(53, 64, 51, 42, 34,42)
cpi <- c(45.7, 46.3, 47.3, 48.4, 49.5, 50.60)
gdp <- c(80.63, 80.90, 82.40, 83.38, 84.38, 85.17)
dum1 <- c(0,0,1,0,0,0)
dum2 <- c(0,0,0,1,0,0)
dum3 <- c(1,0,0,0,1,0)
dx <- c(6.39, 6,6.57, 5.84, 6.36, 5.78)

test <- data.frame(u,cci,bci,cpi,gdp,dum1,dum2,dum3,dx)

library(forecast)

x <- arima(test$dx, order= c(3,1,1), xreg=test[,1:4])

x

y <- arima(test$dx, order= c(1,0,0))

y