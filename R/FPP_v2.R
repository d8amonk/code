#####################################
####        LOADING AREA          ###
#####################################

#Much libraries
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("fpp", "forecast", "lattice", "ggplot2", "dplyr", "timeSeries", "tseries", "xts"))
remove(libraries)
#So data
setwd("c:/Users/Jeffrey/Google Drive/cDOT/") 
d.0 <- d <- read.csv("Query3_v2.csv", header = T) 

#Barney-style:
ID <- c("A", "B", "B", "c", "A", "B", "c", "c", "A", "B")
Date = seq(as.Date("2000/07/01"), as.Date("2000/07/10"), "days")
Amt <- rnorm(10, 10, 3)

E <- data.frame(Date = Date, ID = ID, Amt = Amt)
E
E.e <- E %>%
  group_by(ID) %>% 
  summarise(contract_len = as.numeric(difftime(last(Date), first(Date), unit="days")),
            first_pay = first(Date),
            last_pay =last(Date),
            #flightpath = list(d=Date, p=Amt),
            num_payments = n(),
            payment = sum(Amt))

#####################################
####   BUILDING & TRAINING AREA   ###
#####################################

#Build a temp_Date object using the date features of each contract
temp_Date <- as.Date(paste(d$Month,d$Day,d$Year,sep="/"), format="%m/%d/%Y")

#Eliminate unused columns 
d <- subset(d, select = -c(FedContractNum, Lev2OfficeNum, EstNum, Appr_Dt, Appr_Dt2, Loc, Month, Day, Year))

# check for NAs (should return a "0-length row.names" object)
d[!complete.cases(d),] 

#make a data.frame with an ISO date column
exp <- data.frame(d, Date = temp_Date)

#LOOK at the data
head(exp, 10)

#Using dplyr, build contract length, num payments, and total payments 
#(REPLAcES PIVOT SEcTIONS)
exp.c <- exp %>% 
  group_by(ID) %>% 
  summarise(c_start = as.POSIXct(min(Date)),
            c_end = as.POSIXct(min(Date)),
            c_len = as.numeric(difftime(last(Date), first(Date), unit="days")),
            c_Len_Months = c_Len_Days/4.33, # 52/12 = 4.33wks/month
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
fit1 <- lm(c_Len_Days ~ c_Tot, exp.c)
fit2 <- lm(c_Num_Payments ~ c_Tot, exp.c)

#set graphic device output for comparison
par(mfrow = c(2,1))

#comparison of plots with PRFs overlaid
plot(exp.c$c_Tot, 
     exp.c$c_Len_Days,
     pch = 1,
     xlab = "Millions", 
     ylab = "contract Length in Days", 
     main = "Length = a + B.Millions?")
     #,
     #col=rgb(100,0,0,80,maxcolorValue=255))
abline(lm(c_Len_Days ~ c_Tot, exp.c), col="red", lwd = 3, lty = 3)
plot(exp.c$c_Tot, 
     exp.c$c_Num_Payments, 
     xlab = "Millions", 
     ylab = "contract Payments", 
     main = "Payments = a + B.Millions?")
abline(lm(c_Num_Payments ~ c_Tot, exp.c), col="red", lwd = 3, lty = 3)
#reset graphics device
dev.off()

# library(car)
# scatterplot(c_Len_Wks ~ c_Tot | c_Num_Payments, data=exp.c, 
#             xlab="Total contract Value", ylab="contract Length in Weeks", 
#             main="Enhanced Scatter Plot")

exp_xts <- xts(exp$Amount, order.by=exp$Date)
exp_xts <- xts(exp.c, order.by = exp.c)
plot.xts(exp_xts$Date, exp_xts$Amount, xlab = "Date", ylab = "Amount")

exp %>% group_by(Date) %>% summarise_each(funs(sum))
dd <- aggregate(Amount ~ Date, exp, sum)


m_exp_0014 <- read.csv("expense_m_0007_1412_r174_c2.csv", header = F)
m_amt <- m_exp_0014[,2]
m_Date = seq(as.Date("2000/07/01"), as.Date("2014/12/31"), "months")
m_df <- data.frame(Date = ts(m_Date, frequency = 12), Amount = ts(as.numeric(m_amt),start = c(2000,7), end = c(2014,12), frequency = 12))
m_ts <- ts(m_df$Amount, start = c(2007,1), end = c(2014,12), frequency = 12)
#####################################
####         FITTING AREA         ###
#####################################
#WORKS
#fit <- tbats(exp$Amount, use.trend= TRUE, use.box.cox = TRUE, seasonal.periods = c(12))

fit <- stl(m_ts, t.window = 5, s.window="periodic",robust = T)
plot(fit)

#(these are garbage models; placeholders for ANOVA)
fit_Y <- lm(Amount ~ Year, exp)
fit_D <- lm(Amount ~ Day, exp)
fit_M <- lm(Amount ~ Month, exp)
fit_count_Date <- lm(Amount ~ Date + count, exp)

summary(fit_Y)
summary(fit_D)
summary(fit_M)
summary(fit_count_Date)

anova(fit_Y,fit_D,fit_M,fit_count_Date)


#####################################
####       PLOTTING AREA          ###
#####################################
qplot(exp$Date, exp$Amount/1000000, color = exp$ID, xlab = "Year", ylab = "Payment Amount ($M)", ggtitle = "contractors over Years", alpha = 0.1)
qplot(exp$Date, exp$Amount/1000000, color = exp$ID, xlab = "Year", ylab = "Payment Amount ($M)", ggtitle = "contractors over Years", alpha = 0.1)
fit_d <- stl(m_df$Amount, s.window = 5)
plot(fit_d)
seasonplot(m_ts, ylab="$Million", xlab="Year",main="Seasonal Plot: cDOT Expenditures, 2000-2014")

#seasonal deviation
monthplot(m_ts,ylab="$ million",xlab="Month",xaxt="n",
          main="Monthly Deviations from Means, 2000-14")
axis(1, at=1:12, labels=month.abb, cex=0.8)
#axis(2, at=1:50)
#par(mfrow=c(2,2))
plot(m_ts, main = "Monthly cDOT Expenditure", xlab="Time", ylab="Millions")
#plot(diff(d) ,xlab="Day",ylab="Monthly change in Expenditure")

dev.off()

fit <- stl(m_ts, s.window = 5)
plot(m_ts, col=16,
     main="Monthly cDOT Expenditure, 2000-2015",
     ylab="Millions", xlab="Time", )
lines(fit$time.series[,2],col=15,lty = 5,lwd = 3)
lines(seasadj(fit),col=17)

plot(fit)

plot(m_ts, col="grey",
     main="Monthly cDOT Expenditure, 2000-2015",
     xlab="", ylab="Millions")
lines(seasadj(fit),
      col="red",
      ylab="Seasonally Adj.")
plot(ma(m_ts, order = 15))


#6.2
plot(m_ts, main="Monthly cDOT Expenditure, 2000-2015",
     ylab="Millions", xlab="Time")
lines(ma(m_ts,12),col="red") #>5 = smoother trendline
#same
plot(m_ts, ylab="Index", col="gray",
     main="Monthly cDOT Expenditure, 2000-2014")
lines(ma(m_ts, order=12), col="red")
# combinations of moving averages 
# result in weighted moving averages.

#same type of interpretations:
#NOT WORKING
d2 <- window(m_ts)#, start=2000)
plot(d2, ylab = "Millions")
ma4 <- ma(d2, order=5)
plot(ma4)

# a 2*�12-MA can be used to estimate the trend-cycle of
# monthly data and a 7-MA can be used to estimate the 
# trend-cycle of daily data

#6.3
fit <- decompose(m_ts, type="multiplicative")
plot(fit)

#2.2 autocorr? NOT AcTUAL DATA USED HERE (DON'T WORRY ABT AcF!!!!)
dl <- window(m_ts)#, start=2000)
lag.plot(dl, set.lags=c(6:12), do.lines=FALSE, diag.col="red")
lag.plot(dl, set=12, do.lines=FALSE, diag.col="red")
acf(dl)

acf(dd,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

#'WHITENOISE':
x <- ts(rnorm(50))
plot(x, main="White noise")
acf(x)


#6.4
#x-12-arima; see x12 package

#6.5
# an alternative STL decomposition where the trend 
# is more flexible, the seasonal component does not 
# change over time, and the robust option has been used

fit <- stl(m_ts, t.window=15, s.window="periodic", robust=FALSE)
plot(fit)

#6.6
fit <- stl(m_ts, t.window=36, s.window=24, robust=T)
plot(fit)
d_adj <- seasadj(fit)
plot(naive(d_adj), ylab="Expenditure Index",
  main="Naive Forecasts of Seasonally Adjusted Data")

x <- forecast(fit, method="naive", level = c(75,99))
plot(x, ylab="Millions")
str(x)

#####################################
####           GRAVEYARD          ###
#####################################

# #cheap way to build-out a payment space ***FILLS WITH 0s in Amount***
# cTS_date_V <- data.frame(Date = seq(as.Date("2000/07/01"), as.Date("2014/12/31"), "days"))
# exp_d <- merge(exp, cTS_date_V, by="Date", all.y = T)
# exp_d$Amount[is.na(exp_d$Amount)] <- 0

#d_exp_0014 <- read.csv("expense_counts_0014.csv", col.names = c("Year","Month","Day","Amount", "count"), header = F)

# m_exp_0014 <- read.csv("expense_m_0007_1412_r174_c2.csv", header = F)
# m_amt <- m_exp_0014[,2]
# Date = seq(as.Date("2000/07/01"), as.Date("2014/12/31"), "months")
# d = m_df <- data.frame(Date = ts(start = c(2000,7), end = c(2014,12), frequency = 12), Amount = ts(as.numeric(m_amt),start = c(2000,7), end = c(2014,12), frequency = 12))

# d_exp_0014 <- timeSeries(read.csv("expense_d_0007_1412_r3134_c4.csv", header=F),
#         start = c(2000,7),
#         end = c(2014,12),
#         frequency = 365)
# d <- ts(d, 
#         start=c(2000,1),
#         frequency = 12)


#####################################
####             NOTES            ###
#####################################
# NIcE cOLUMN DROP EG:
#df <- subset( df, select = -c(x, y, z) )
#pay$obs <- c(1:174)
#pay <- subset
#m_exp_0014 <- xts(read.csv("expense_m_0007_1412_r174_c2.csv", header = F))

# FOR LATER AUTOMATION: 
#if(!require(data.table)){install.packages("data.table")}

# NIcE cHEcK ON THE DATA:
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
