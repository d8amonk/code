library(fpp, forecast)

#6.1
d <- ts(read.csv("deseason_vVectForTS.csv", 
                 header = FALSE),
        start=c(2004,1), 
        end=c(2012,12), 
        frequency = 12)

#par(mfrow=c(2,2))
plot(d, main = "Monthly CDOT Expenditure", xlab="Time", ylab="Millions")
#plot(diff(d) ,xlab="Day",ylab="Monthly Change in Expenditure")

dev.off()

fit <- stl(d, s.window=5)
plot(d, col="gray",
     main="Monthly CDOT Expenditure, 2004-2011",
     ylab="Millions", xlab="Time")
lines(fit$time.series[,2],col="red",ylab="Trend")

plot(fit)
dd <- d/1000000
fit_dd <- stl(dd, s.window=5)

plot(dd, col="grey",
     main="Monthly CDOT Expenditure, 2004-2011",
     xlab="", ylab="Millions")
lines(seasadj(fit_dd),
      col="red",
      ylab="Seasonally Adj.")

plot(ma(d, order=5))


#6.2
plot(d, main="Monthly CDOT Expenditure, 2004-2011",
     ylab="Millions", xlab="Time")
lines(ma(d,5),col="red") #>5 = smoother trendline

d2 <- window(d, start=2004)
plot(d2)
ma4 <- ma(d2, order=4, center = FALSE)
plot(ma4)
ma2x4 <- ma(d2, order=4, center = TRUE)
plot(ma2x4)
# a 2×12-MA can be used to estimate the trend-cycle of
# monthly data and a 7-MA can be used to estimate the 
# trend-cycle of daily data

plot(d, ylab="Index", col="gray",
     main="Monthly CDOT Expenditure, 2004-2011")
lines(ma(d, order=12), col="red")
# Combinations of moving averages 
# result in weighted moving averages.

#6.3
fit <- decompose(d, type="multiplicative")
plot(fit)

#6.4
#x-12-arima; see x12 package

#6.5
# an alternative STL decomposition where the trend 
# is more flexible, the seasonal component does not 
# change over time, and the robust option has been used

fit <- stl(d, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)