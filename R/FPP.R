library(fpp, forecast)

#6.1
d <- ts(read.csv("deseason_vVectForTS.csv", header=FALSE)[,1],
        start = c(2004,1),
        end = c(2010,12),
        frequency = 12)
# d <- ts(d,
 #         start=c(2004,1),
#         frequency = 12)
dd <- d/1000000
fit_dd <- stl(dd, s.window=5)

seasonplot(dd,ylab="$Million", xlab="Year", 
           main="Seasonal Plot: CDOT Expenditures, 2004-2011",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=15)
#col=1:20 means the chart can accomodate 20 years with unique coloring
#but in this case, only 7 are used

#seasonal deviation
monthplot(dd,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: CDOT Expenditures, 2004-2011")
axis(1,at=1:12,labels=month.abb,cex=0.8)
#par(mfrow=c(2,2))
plot(d, main = "Monthly CDOT Expenditure", xlab="Time", ylab="Millions")
#plot(diff(d) ,xlab="Day",ylab="Monthly Change in Expenditure")

dev.off()

fit <- stl(d, s.window = 5)
plot(d, col="gray",
     main="Monthly CDOT Expenditure, 2004-2011",
     ylab="Millions", xlab="Time")
lines(fit$time.series[,2],col="red",ylab="Trend")

plot(fit)

plot(dd, col="grey",
     main="Monthly CDOT Expenditure, 2004-2011",
     xlab="", ylab="Millions")
lines(seasadj(fit_dd),
      col="red",
      ylab="Seasonally Adj.")

plot(ma(dd, order=6))


#6.2
plot(dd, main="Monthly CDOT Expenditure, 2004-2011",
     ylab="Millions", xlab="Time")
lines(ma(dd,12),col="red") #>5 = smoother trendline

#same type of interpretations:
d2 <- window(dd, start=2004)
plot(d2, ylab = "Millions")
ma4 <- ma(d2, order=5)
plot(ma4)
ma2x4 <- ma(d2, order=4, center = TRUE)
plot(ma2x4)
# a 2*—12-MA can be used to estimate the trend-cycle of
# monthly data and a 7-MA can be used to estimate the 
# trend-cycle of daily data

plot(dd, ylab="Index", col="gray",
     main="Monthly CDOT Expenditure, 2004-2011")
lines(ma(dd, order=12), col="red")
# Combinations of moving averages 
# result in weighted moving averages.

#6.3
fit <- decompose(dd, type="multiplicative")
plot(fit)

#2.2 autocorr?
dl <- window(dd, start=2004)
lag.plot(dl, set.lags=c(6:12), do.lines=FALSE, diag.col="red")
lag.plot(dl, set=12, do.lines=FALSE, diag.col="red")
acf(dl)

acf(dl,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

#think:
set.seed(45)
x <- ts(rnorm(50))
plot(x, main="White noise")
acf(x)


#6.4
#x-12-arima; see x12 package

#6.5
# an alternative STL decomposition where the trend 
# is more flexible, the seasonal component does not 
# change over time, and the robust option has been used

fit <- stl(dd, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)

#6.6
fit <- stl(dd, t.window=15, s.window="periodic", robust=FALSE)
d_adj <- seasadj(fit)
plot(naive(d_adj), ylab="Expenditure Index",
  main="Naive Forecasts of Seasonally Adjusted Data")

fcast <- forecast(fit, method="naive", level = c(75,99))
plot(fcast, ylab="Millions")
