#####################################
####        LOADING AREA          ###
#####################################

# Much libraries
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("fpp", "forecast", "lattice", "ggplot2", "dplyr", "timeSeries", "tseries", "xts", "data.table"))
remove(libraries)

# So data
# setwd("") 
d.0 <- d <- read.csv("c:/Users/Jeffrey/Google Drive/CDOT/Data/Jason Pulls/Query3_v2.csv", header = T) 
options(scipen=999)
#####################################
####   BUILDING & TRAINING AREA   ###
#####################################

#Build a temp_Date object using the date features of each contract
temp_Date <- as.Date(paste(d$Month,d$Day,d$Year,sep="/"), format="%m/%d/%Y")

#Eliminate unused columns 
d <- subset(d, select = -c(FedContractNum, Lev2OfficeNum, EstNum, Appr_Dt, Appr_Dt2, Loc, Month, Day, Year))

# check for NAs (should return a "0-length row.names" object)
summary(d[!complete.cases(d),])

#make a data.frame with an ISO date column
exp <- data.frame(d, Date = temp_Date)
#exp <- exp %>% group_by(Date) %>% summarise(funs(sum))
dd <- aggregate(Amount ~ Date, exp, sum)

#LOOK at the data
head(exp, 10)

#Using dplyr, build contract length, num payments, and total payments 
#(REPLAcES PIVOT SEcTIONS)
exp.c <- exp %>% 
  group_by(ID) %>% 
  summarise(c_start = as.Date(min(Date)),
            c_end = as.Date(max(Date)),
            c_len = as.numeric(difftime(last(Date), first(Date), unit="days")),
            c_Len_Months = c_len/30, # 52/12 = 4.33wks/month
            c_Num_Payments = n(),
            c_Tot = sum(Amount))
#aggregator functions within summarise()
#min(x) - minimum value of vector x.
#max(x) - maximum value of vector x.
#mean(x) - mean value of vector x.
#median(x) - median value of vector x.
#quantile(x, p) - pth quantile of vector x.
#sd(x) - standard deviation of vector x.
#var(x) - variance of vector x.
#IQR(x) - Inter Quartile Range (IQR) of vector x


#fitting two linear models
fit1 <- lm(c_len ~ c_Tot, exp.c)
fit2 <- lm(c_Num_Payments ~ c_Tot, exp.c)

#comparison of plots with PRFs overlaid
dev.off()
par(mfrow = c(2,1))

require(lattice)
plot(exp.c$c_Tot,
     exp.c$c_len,
     pch = 1,
     xlab = "$$$", 
     ylab = "# Days", 
     main = "Length = a + B.Millions?")
     #col=rgb(1,0,0,.8)), maxcolorValue=255?
abline(lm(c_len ~ c_Tot, exp.c), col="red", lwd = 2, lty = 1)
plot(exp.c$c_Tot, 
     exp.c$c_Num_Payments, 
     xlab = "$$$", 
     ylab = "# of Payments", 
     main = "Payments = a + B.Millions?")
abline(lm(c_Num_Payments ~ c_Tot, exp.c), col="red", lwd = 2, lty = 1)
#reset graphics device
dev.off()

# library(car)
# scatterplot(c_Len_Wks ~ c_Tot | c_Num_Payments, data=exp.c, 
#              xlab="Total contract Value", ylab="contract Length in Weeks", 
#              main="Enhanced Scatter Plot")

exp_xts <- xts(exp$Amount,  order.by=exp$Date)
plot.xts(exp_xts, xlab = "Date", ylab = "Amount")

exp <- exp %>% group_by(Date) %>% summarise_each(funs(sum))
exp.d <- exp %>% select(Amount) %>% as.ts(order.by = Date)
dd <- aggregate(Amount ~ Date, exp, sum)
nrow(exp) == nrow(dd)

require(data.table)
m_exp_0014 <- read.csv("c:/Users/Jeffrey/Google Drive/CDOT/Data/Jason Pulls/expense_m_0007_1412_r174_c2.csv", header = F)
m_amt <- data.table(m_exp_0014[,2])
m_Date = seq(as.Date(11504), as.Date(16435), "months")
m_df <- data.frame(Date = ts(m_Date, frequency = 12), Amount = ts(as.numeric(m_amt), start = as.Date(11504), end = as.Date(16435), frequency = 12))
m_ts <- ts(m_df$Amount, start = c(2000,7), end = c(2014,12), frequency = 12)
m_dt <- as.data.table(m_df)
m_dt <- m_dt$Date  %>% filter(Date > 14610)


#####################################
####         FITTING AREA         ###
#####################################
#WORKS
#fit <- tbats(exp$Amount, use.trend= TRUE, use.box.cox = TRUE, seasonal.periods = c(12))

fit3 <- stl(m_ts, 
           s.window = 12, s.degree = 1,
           t.window = 12, t.degree = 1,
           l.window = 12, l.degree = 1)
plot(fit3)

acf(m_ts, 
    lag.max = 36,
    plot = TRUE, 
    na.action = na.pass, 
    demean = TRUE)

# (these are not production-grade; placeholders for ANOVA)
# fit_Y <- lm(Amount ~ Year, exp)
# fit_D <- lm(Amount ~ Day, exp)
# fit_M <- lm(Amount ~ Month, exp)
# fit_count_Date <- lm(Amount ~ Date + count, exp)
# 
# summary(fit_Y)
# summary(fit_D)
# summary(fit_M)
# summary(fit_count_Date)
# 
# anova(fit_Y,fit_D,fit_M,fit_count_Date)


#####################################
####       PLOTTING AREA          ###
#####################################
qplot(exp$Date, exp$Amount, color = exp$MonthTotal, xlab = "Year", ylab = "Payment Amount ($M)", ggtitle = "contractors over Years")
fit_4 <- stl(m_df$Amount, s.window = 5)
plot(fit_4)
#seasonplot(exp_xts, ylab="$Million", xlab="Year",main="Seasonal Plot: CDOT Expenditures, 2000-2014")

# seasonal deviation
# monthplot(m_ts,ylab="$ million",xlab="Month",xaxt="n", main="Monthly Deviations from Means, 2000-14")
# axis(1, at=1:12, labels=month.abb, cex=0.8)
# axis(2, at=1:50)
# par(mfrow=c(2,2))
plot(m_ts, main = "Monthly CDOT Expenditure", xlab="Time", ylab="Millions")
# plot(diff(d) ,xlab="Day",ylab="Monthly change in Expenditure")

dev.off()

fit5 <- stl(m_ts, s.window = 5)
plot(m_ts, col=16,
     main="Monthly CDOT Expenditure, 2000-2015",
     ylab="Millions", xlab="Time", )
lines(fit5$time.series[,2],col=15,lty = 5,lwd = 3)
lines(seasadj(fit5),col=17)

plot(fit5)

plot(m_ts, col="grey",
     main="Monthly CDOT Expenditure, 2000-2015",
     xlab="", ylab="Millions")
lines(seasadj(fit5),
      col="red",
      ylab="Seasonally Adj.")
plot(ma(m_ts, order = 15))

#6.2
plot(m_ts, main="Monthly CDOT Expenditure, 2000-2015",
     ylab="Millions", xlab="Time")
lines(ma(m_ts,12),col="red") #>5 = smoother trendline
#same
plot(m_ts, ylab="Index", col="gray",
     main="Monthly CDOT Expenditure, 2000-2014")
lines(ma(m_ts, order=12), col="red")
# combinations of moving averages 
# result in weighted moving averages.
d2 <- window(m_ts)#, start=2000)
plot(d2, ylab = "Millions")
ma4 <- ma(d2, order=5)
plot(ma4)

# a 12-MA can be used to estimate the trend-cycle of
# monthly data and a 7-MA can be used to estimate the 
# trend-cycle of daily data

fit6 <- decompose(m_ts, type="multiplicative")
plot(fit6)

dl <- window(m_ts)#, start=2000)
################NOT AcTUAL DATA USED HERE (DONT WORRY ABT ACF!!!!)
##################################################################
lag.plot(dl, set.lags=c(6:12), do.lines=T, diag.col="red", lwd = 3,  main = "SIMULATED DATA")
lag.plot(dl, set=12, do.lines=T, diag.col="red", lwd = 3, main = "SIMULATED DATA")
acf(dl)

acf(dd,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

#'WHITENOISE':
x <- ts(rnorm(50))
plot(x, main="White noise")
acf(x)

#x-12-arima; see x12 package
# an alternative STL decomposition where the trend 
# is more flexible, the seasonal component does not 
# change over time, and the robust option has been used

fit7 <- stl(m_ts,
           s.window = 12, s.degree = 1,
           t.window = 3, t.degree = 1,
           l.window = NULL, l.degree = 1,
           robust = T)
plot(fit7)
acf(m_ts)

# Add seasadj()
fit8 <- stl(m_ts, t.window = 5, s.window=11,  l.window = 13, robust=T)
d_adj <- seasadj(fit8)
acf(d_adj)
plot(d_adj, main = "Annual CDOT Expenditures, Seasonally Adj.")
# plot(naive(d_adj), ylab="Expenditure Index", main="Naive Forecasts of Seasonally Adjusted Data")

x <- forecast(fit7)
plot(x, ylab="Millions")
str(x)

require(data.table)

fc_1517 <- data.table(point_Est = x$mean, upper_Bounds = x$upper, lower_Bounds = x$lower, levels = x$level)

#####################################
####           GRAVEYARD          ###
#####################################

# #cheap way to build-out a payment space ***FILLS WITH 0s in Amount***
# cTS_date_V <- data.frame(Date = seq(as.Date("2000/07/01"), as.Date("2014/12/31"), "days"))
# exp_d <- merge(exp, cTS_date_V, by="Date", all.y = T)
# exp_d$Amount[is.na(exp_d$Amount)] <- 0

# d_exp_0014 <- read.csv("expense_counts_0014.csv", col.names = c("Year","Month","Day","Amount", "count"), header = F)

# m_exp_0014 <- read.csv("expense_m_0007_1412_r174_c2.csv", header = F)
# m_amt <- m_exp_0014[,2]
# Date = seq(as.Date("2000/07/01"), as.Date("2014/12/31"), "months")
# d = m_df <- data.frame(Date = ts(start = c(2000,7), end = c(2014,12), frequency = 12), Amount = ts(as.numeric(m_amt),start = c(2000,7), end = c(2014,12), frequency = 12))

# d_exp_0014 <- timeSeries(read.csv("expense_d_0007_1412_r3134_c4.csv", header=F),
#         start = c(2000,7),
#         end = c(2014,12),
#         frequency = 365)
# d <- ts(d, 
#         start=c(2000,7),
#         frequency = 12)


#####################################
####             NOTES            ###
#####################################

# m_exp_0014 <- xts(read.csv("expense_m_0007_1412_r174_c2.csv", header = F))

# FOR LATER 
# automate: 
# if(!require(data.table)){install.packages("data.table")}

# warm fuzz:
# > 5296-3133
# [1] 2163
# > 16623-14460
# [1] 2163

# ANY APPLIcATION FOR cONTRAcT DRAW-DOWN SHAPE??
# Box office Star Wars: In Millions (!)  First element: US, Second
# # element: Non-US
# a=new_hope <- c(460.998007, 314.4)
# b=empire_strikes <- c(290.475067, 247.9)
# c=return_jedi <- c(309.306177, 165.8)
# 
# # Add code below to construct the matrix
# star_wars_matrix <- matrix(c(a,b,c), nrow=3)
# # Show me the
# star_wars_matrix
