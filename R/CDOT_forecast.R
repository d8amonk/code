#CHECK BOTTOM FOR SCRAPS

#for later automation:  if(!require(data.table)){install.packages("data.table")}

libraries <- function(x) lapply(x, require, character.only=T)
libraries(c("fpp", "forecast", "lattice", "ggplot2", "dplyr", "timeSeries"))



#any applications with CONTRACT draw-down?
# Box office Star Wars: In Millions (!)  First element: US, Second
# # element: Non-US
# a=new_hope <- c(460.998007, 314.4)
# b=empire_strikes <- c(290.475067, 247.9)
# c=return_jedi <- c(309.306177, 165.8)
# 
# # Add your code below to construct the matrix
# star_wars_matrix <- matrix(c(a,b,c), nrow=3)
# # Show me the
# star_wars_matrix


d_exp_0014 <- read.csv("expense_counts_0014.csv", col.names = c("Year","Month","Day","Amount", "Count"), header = F)

d_exp_0014[!complete.cases(d_exp_0014),] # check for NAs
# m[!complete.cases(m),] # check for NAs
# d[!complete.cases(d),]

d <- d_exp_0014
myDate <- as.Date(paste(d$Month,d$Day,d$Year,sep="/"), format="%m/%d/%Y")
remove(d)
exp <- data.frame(d_exp_0014, Date = myDate, t = 1:3133)
# exp_df <- data.frame(Amount = d$Amount, Day = d$Day, Month = d$Month, Year = d$Year)
# exp_df$Date <- as.Date(paste(d$Year, d$Month, d$Day, sep="-"))
str(exp)

fit_Y <- lm(Amount ~ Year, exp)
fit_D <- lm(Amount ~ Day, exp)
fit_M <- lm(Amount ~ Month, exp)
fit_t <- lm(Amount ~ t + Count, exp)
fit_all <- lm(Amount ~ Day + Month + Year + Count + t, exp) #NOTE THAT THIS IS NOT A LINEAR MODEL W 3133 OBS
fit_Count_Date <- lm(Amount ~ Date + Count, exp)

summary(fit_Y)
summary(fit_D)
summary(fit_M)
summary(fit_t)
summary(fit_all)
summary(fit_Count_Date)

anova(fit_Y,fit_D,fit_M,fit_t,fit_all,fit_Count_Date)

CTS_date_V <- as.data.frame(seq(as.Date("2000/07/01"), as.Date("2014/12/31"), "days"), colnames = "Date")

dd <- merge(exp_df, CTS_date_V, by="Date", all.y = T)
dd$Amount[is.na(dd$Amount)] <- 0
#DEPR dd$t <- 1:5297
#REMEMBER: DF$Day $Month $Year all contain NAs!

d_xts <- ts(dd$Amount, start = as.Date(min(dd$Date), end = as.Date(max(dd$Date))))
d_ts <- ts(dd$Amount, start = min(dd$Date), end = max(dd$Date))

fit_dd <- stl(d_ts, s.window=5)

seasonplot(dd,ylab="$Million", xlab="Year", 
           main="Seasonal Plot: CDOT exp_dfenditures, 2000-2015",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=15)
#col=1:20 means the chart can accomodate 20 years with unique coloring
#but in this case, only 7 are used

#seasonal deviation
monthplot(dd,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: CDOT exp_dfenditures, 2000-2015")
axis(1,at=1:12,labels=month.abb,cex=0.8)
#par(mfrow=c(2,2))
plot(dd, main = "Monthly CDOT exp_dfenditure", xlab="Time", ylab="Millions")
#plot(diff(d) ,xlab="Day",ylab="Monthly Change in exp_dfenditure")

dev.off()

fit <- stl(dd, s.window = 5)
plot(dd, col=16,
     main="Monthly CDOT exp_dfenditure, 2000-2015",
     ylab="Millions", xlab="Time", )
lines(fit$time.series[,2],col=15,lty = 5,lwd = 3)
lines(seasadj(fit_dd),col=17)

plot(fit)

plot(dd, col="grey",
     main="Monthly CDOT exp_dfenditure, 2000-2015",
     xlab="", ylab="Millions")
lines(seasadj(fit_dd),
      col="red",
      ylab="Seasonally Adj.")

plot(ma(dd, order=6))


#6.2
plot(dd, main="Monthly CDOT exp_dfenditure, 2000-2015",
     ylab="Millions", xlab="Time")
lines(ma(dd,12),col="red") #>5 = smoother trendline
#same
plot(dd, ylab="Index", col="gray",
     main="Monthly CDOT exp_dfenditure, 2000-2014")
lines(ma(dd, order=12), col="red")
# Combinations of moving averages 
# result in weighted moving averages.

#same type of interpretations:
#NOT WORKING
d2 <- window(dd, start=2000)
plot(d2, ylab = "Millions")
ma4 <- ma(d2, order=5)
plot(ma4)
ma2x4 <- ma(d2, order=4, center = TRUE)
plot(ma2x4)
# a 2*—12-MA can be used to estimate the trend-cycle of
# monthly data and a 7-MA can be used to estimate the 
# trend-cycle of daily data



#6.3
fit <- decompose(dd, type="multiplicative")
plot(fit)

#2.2 autocorr? NOT ACTUAL DATA USED HERE (DON'T WORRY ABT ACF!!!!)
dl <- window(dd, start=2000)
lag.plot(dl, set.lags=c(6:12), do.lines=FALSE, diag.col="red")
lag.plot(dl, set=12, do.lines=FALSE, diag.col="red")
acf(dl)

acf(dd,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

#think:
x <- ts(rnorm(50))
plot(x, main="White noise")


acf(x)


#6.4
#x-12-arima; see x12 package

#6.5
# an alternative STL decomposition where the trend 
# is more flexible, the seasonal component does not 
# change over time, and the robust option has been used

fit <- stl(dd, t.window=15, s.window="periodic", robust=FALSE)
plot(fit)

#6.6
fit <- stl(dd, t.window=36, s.window=24, robust=T)
plot(fit)
d_adj <- seasadj(fit)
plot(naive(d_adj), ylab="exp_dfenditure Index",
     main="Naive Forecasts of Seasonally Adjusted Data")

x <- forecast(fit, method="naive", level = c(75,99))
plot(x, ylab="Millions")
str(x)

#check dependencies for automation:  if(!require(data.table)){install.packages("data.table")}
#a$x[is.na(a$x)] <- 0

#drop cols
#df <- subset( df, select = -c(x, y, z) )
#pay$obs <- c(1:174)

# #ANOVA block
# fit_Y <- lm(Amount ~ Year)
# fit_D <- lm(Amount ~ Day)
# fit_M <- lm(Amount ~ Month)
# fit_t <- lm(Amount ~ t)
# fit_all <- lm(Amount ~ Day + Month + Year)
# summary(fit_Y)
# summary(fit_D)
# summary(fit_M)
# summary(fit_t)
# summary(fit_all)
# anova(fit_Y,fit_D,fit_M,fit_t,fit_all)

# m_exp_df_0014 <- ts(read.csv("exp_dfense_m_0007_1412_r174_c1.csv", header = F)[,1],
#                     start=c(2000,7),
#                     end=c(2014,12),
#                     frequency=12)
# d_exp_df_0014 <- timeSeries(read.csv("exp_dfense_d_0007_1412_r3134_c4.csv", header=F),
#         start = c(2000,7),
#         end = c(2014,12),
#         frequency = 365)
# d <- ts(d, 
#         start=c(2000,1),
#         frequency = 12)
