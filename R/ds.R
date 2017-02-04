d <- ts(read.csv("d_v3.csv"),
        start = c(2004,1),
        end = c(2011,12),
        frequency = 1)2

#trend-cycle component
fit <- stl(d, s.window=5)
plot(d)
lines(fit$time.series[,2],col="red")

#seasonally adjusted data
plot(d, 
     col="grey",
     main="Highway Construction Payments",
     ylab="$")
dd <- d/1000000
plot(dd, col="grey",
     main="Highway Construction Payments",
     ylab="$")

lines(seasadj(fit),
      col="red",
      ylab="Seasonally adjusted")

#seasonal component of variation by month
monthplot(fit$time.series[,"seasonal"],
          main="",
          ylab="Seasonal")
