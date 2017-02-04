//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
///   ***this is the CONTRACT ECOLOGY section***   ///
///    THIS SECTION RUNS ON data.table() OBJECTS   ///
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////


#####################################
####        STAGING AREA          ###
#####################################
# Data...
#         H O M E
zj_40 <- read.csv("c:/Users/Jeffrey/Google Drive/CDOT/Data/Reports Data/ZJ40/Zj40_v1.csv", header = T) 
#         W O R K
zj_40 <- read.csv("c:/Users/vangeetej/Google Drive/CDOT/Data/Reports Data/ZJ40/Zj40_v1.csv", header = T) 
# 4852 obs. of  40 variables, *9 unique date columns! ;(

#         H O M E
# cj <-  read.csv("c:/Users/Jeffrey/Google Drive/CDOT/Data/Reports Data/CJI40/Cji4_v1.csv", header = T)
#         W O R K
# cj <-  read.csv("c:/Users/vangeetej/Google Drive/CDOT/Data/Reports Data/CJI40/Cji4_v1.csv", header = T)

# 1848 obs. of  23 variables, *payout grid :)

# Libs...
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("fpp", "forecast", "lattice", "ggplot2", "dplyr", "timeSeries", "tseries", "xts"))
remove(libraries)


#####################################
####   BUILDING & TRAINING AREA   ###
#####################################

# Build a temporary date object using the date features of each contract
temp_Date <- as.Date(paste(zj_40$Month,zj_40$Day,zj_40$Year,sep="/"), format="%m/%d/%Y")
# month_vector = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Rs
zj <- zj_40 %>% 
  mutate(REF.Len = abs(REFBEG-REFEND), 
         cost.spread = as.numeric(as.numeric(Budget)-as.numeric(Expenditure))) %>%
  select(ID = PROJECT.Def,
         WBS,
         rudemc = Funded.program,
         Phase, 
         Region, 
         Budget, 
         Expenditure,
         cost.spread, #dbl
         len = REF.Len,
         aw.date = Award.Date, #FACTOR: gets coerced to Date below
         ad.date = Initial.AD.Date, #FACTOR: gets coerced to Date below
         status = Status.Code)

origin = as.Date("01/01/1970",format = "%m/%d/%Y", origin = "01/01/1970")
zj$aw.date <- as.Date(zj$aw.date, format = "%m/%d/%Y", origin)
zj$ad.date <- as.Date(zj$ad.date, format = "%m/%d/%Y", origin)

zj <- zj %>% 
  mutate(ad.award.spread = difftime(aw.date, ad.date, unit = 'days'))

cs <- zj %>% 
  select(ID, cost.spread, ad.award.spread) %>% 
  filter (abs(cost.spread) > mean(cost.spread) + sd(cost.spread))
summary(cs)
qplot(data = cs, cost.spread, ad.award.spread)

# plot(zj$cost.spread, zj$ad.award.spread) #worth exploring with a subset of the largest spreads
# glimpse(zj)
# View(zj)

# Filter 1
exp.f1 <- exp.c %>% 
  filter(!is.na(Date),
         !is.na(Date),
         Ex != 0, Budget < 100000000, 
         Expenditure != 0, Expenditure < 100000000)
# as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))

# recode 99 to missing for variable v1
# select rows where v1 is 99 and recode column v1 
# mydata$v1[mydata$v1==99] <- NA #keep "" days, tagged with NA for filter
# mydata[!complete.cases(mydata),]


# make a data.frame with an ISO date column
# zj <- data.frame(zj, Date = temp_Date)

p.b_lessthan_e <- mean(zj_f1$Budget < zj_f1$Expenditure)*100 # 1.329535%
p.b_equals_e <- mean(zj_f1$Budget == zj_f1$Expenditure)*100 # 12.44065%
p.b_morethan_e <- mean(zj_f1$Budget > zj_f1$Expenditure)*100 # 86.22982%
sum(p.b_lessthan_e + p.b_equals_e + p.b_morethan_e) #SHOULD SUM TO 100

qplot(zj_f1$aw.date, 
      zj_f1$Expenditure, 
      ylim = c(0,6e+7))

#THE DATA TABLE WAY, *streams of payments, contract length, num_payments
require(data.table)
exp.dt <- as.data.table(exp.c) # or convert in place using setDT

exp.sum_c <- exp.dt[, .(contract_len = as.numeric(difftime(aw.date[.N], aw.date[1], unit = 'days')),
       first_pay = aw.date[1],
       last_pay = aw.date[.N],
       num_payments = .N,
       payment = sum(Expenditure),
       over = ifelse(zj_f1$Spread > 0, 0, 1)),
       by = ID]

zj_s <- exp.dt[, .(streams = list(data.table(aw.date, Expenditure, Budget))), 
              key = ID]

zj_s$streams[1:5] #All complete payment curves for 497 contracts?

#CHECK: both the data.table exp.dt and data.frame zj_f1 have the same number of rows by this point.
nrow(zj_f1) == nrow(exp.dt) #TRUE?
summary(exp.dt)


######################
### PROJECT FILTER ###
######################

# Snippits
# dx <- density(exp.c)
# plot(dx)
# dx.f <- factor(exp.c$num_payments, 
#                levels= range(exp.c$num_payments))
# 
# require(sm)
# sm.density.compare(exp.c, num_payments, xlab="Miles Per Gallon")
# title(main="MPG Distribution by Car Cylinders")
# 
# 
# 
# dx <- data.frame(x = d_x$x, y = d_x$y, bw = d_x$bw, n = d_x$n, data.name = d_x$data.name, has.na = d_x$has.na)
# 
# m <- ggplot(dx, aes(x = x))
# m + geom_density(aes(x = x, fill=factor(n)), size=2)


# THINKING: mutate(players, G_delta = G - lag(G))

# TESTING:
# set.seed(1)
# d <- data.frame(a = rnorm(100), b = rnorm(100, 1), c = rnorm(100, 2),
#                 d = rnorm(100, 3), e = rnorm(100, 4))
# library(lattice)
# densityplot(~ a + b + c + d + e, data = d, auto.key = T)
# d2 <- stack(d)
# library(ggplot2)
# qplot(values, colour=factor(ind), data=d2, geom="density", plot.points = T)
# right <- mutate(scrambled, running = order_by(year, cumsum(value)))
# arrange(right, year)
# 
# compute a z-score:
# mutate(players, G_z = (G - mean(G)) / sd(G))


q <- quantile(exp.c$c_tot,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
# plot(q)

q1 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(0 < c_tot, c_tot < q[1]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()                                          
q2 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[1] < c_tot, c_tot < q[2]) %>% summarize(n = n(),  avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q3 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[2] < c_tot, c_tot < q[3]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q4 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[3] < c_tot, c_tot < q[4]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q5 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[4] < c_tot, c_tot < q[5]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q6 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[5] < c_tot, c_tot < q[6]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q7 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[6] < c_tot, c_tot < q[7]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q8 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[7] < c_tot, c_tot < q[8]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q9 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[8] < c_tot, c_tot < q[9]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()
q10 <- exp.c %>% select(c_tot, len_months, num_payments) %>%  filter(q[9] < c_tot, c_tot < q[10]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(c_tot)) %>% data.frame()

contract_slices = rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10)

# contract_slices = data.table(rownames(c('q05', 'q15', 'q20', 'q25', 'q30', 'q35', 'q40', 'q45', 'q50', 'q60', 'q65', 'q70', 'q75', 'q80', 'q90', 'q95')))
cs_sum_temp <- summarise(contract_slices, n = sum(n), avg_len_months = mean(avg_len_months), avg_tot = mean(avg_tot))
meta_contract <- rbind(contract_slices, cs_sum_temp)

rownames(meta_contract) <- c('q1','q2','q3','q4','q5','q6','q7','q8','q9','q10', 'summary')

write.csv(meta_contract, "meta_deciles")


zj_u5 <- zj_f1 %>% 
  # filter(Budget < 5,000,000 | Region == 1 | Phase == 1) %>%
  filter(Budget < 5,000,000 & Region == 1 | Phase == 1) %>%
  summarise(n_u5 = n(),
            p_non = n_u5 / nrow(zj_f1) * 100,
            n_dest = n_distinct(Dest),
            min_dist = min (Distance),
            max_dist = max(Distance)
            )

#USING THE DATA.TABLE FOR PROGRESS PAYMENTS
library(data.table)
dt = as.data.table(zj_f1) # or convert in place using setDT

dt[, .(contract_len = as.numeric(difftime(aw.date[.N], aw.date[1], unit = 'days')),
       first_pay = aw.date[1],
       last_pay = aw.date[.N],
       num_payments = .N,
       payment = sum(Expenditure),
       summary = list(data.table(aw.date, Expenditure)))
    , by = ID]

# fitting two linear models
fit1 <- lm(c_len ~ c_Tot, exp.c)
fit2 <- lm(c_Num_Payments ~ c_Tot, exp.c)

# set graphic device output for comparison
dev.off()
par(mfrow = c(2,1))

# comparison of plots with PRFs overlaid
require(lattice)
plot(exp.c$c_len,
     exp.c$c_Tot,
     pch = 1,     
     xlab = "Millions", 
     ylab = "contract Length in Days", 
     main = "Length = a + B.Millions?")
     # col=rgb(1,0,0,.8)), maxcolorValue=255?
abline(lm(c_Tot ~ c_len, exp.c), col="red", lwd = 2, lty = 1)
plot(exp.c$c_Tot, 
     exp.c$c_Num_Payments, 
     xlab = "Millions", 
     ylab = "contract Payments", 
     main = "Payments = a + B.Millions?")
abline(lm(c_Num_Payments ~ c_Tot, exp.c), col="red", lwd = 2, lty = 1)
# reset graphics device
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
# WORKS
# fit <- tbats(exp$Amount, use.trend= TRUE, use.box.cox = TRUE, seasonal.periods = c(12))

fit <- stl(m_ts, t.window = 5, s.window="periodic",robust = T)
plot(fit)

# (these are garbage models; placeholders for ANOVA)
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

# seasonal deviation
monthplot(m_ts,ylab="$ million",xlab="Month",xaxt="n",
          main="Monthly Deviations from Means, 2000-14")
axis(1, at=1:12, labels=month.abb, cex=0.8)
# axis(2, at=1:50)
# par(mfrow=c(2,2))
plot(m_ts, main = "Monthly cDOT Expenditure", xlab="Time", ylab="Millions")
# plot(diff(d) ,xlab="Day",ylab="Monthly change in Expenditure")

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


# 6.2
plot(m_ts, main="Monthly cDOT Expenditure, 2000-2015",
     ylab="Millions", xlab="Time")
lines(ma(m_ts,12),col="red") #>5 = smoother trendline
# same
plot(m_ts, ylab="Index", col="gray",
     main="Monthly cDOT Expenditure, 2000-2014")
lines(ma(m_ts, order=12), col="red")
# combinations of moving averages 
# result in weighted moving averages.

# same type of interpretations:
# NOT WORKING
d2 <- window(m_ts)#, start=2000)
plot(d2, ylab = "Millions")
ma4 <- ma(d2, order=5)
plot(ma4)

# a 2*�12-MA can be used to estimate the trend-cycle of
# monthly data and a 7-MA can be used to estimate the 
# trend-cycle of daily data

# 6.3
fit <- decompose(m_ts, type="multiplicative")
plot(fit)

# 2.2 autocorr? NOT AcTUAL DATA USED HERE (DON'T WORRY ABT AcF!!!!)
dl <- window(m_ts)#, start=2000)
lag.plot(dl, set.lags=c(6:12), do.lines=FALSE, diag.col="red")
lag.plot(dl, set=12, do.lines=FALSE, diag.col="red")
acf(dl)

acf(dd,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

# 'WHITENOISE':
x <- ts(rnorm(50))
plot(x, main="White noise")
acf(x)


# 6.4
# x-12-arima; see x12 package

# 6.5
# an alternative STL decomposition where the trend 
# is more flexible, the seasonal component does not 
# change over time, and the robust option has been used

fit <- stl(m_ts, t.window=15, s.window="periodic", robust=FALSE)
plot(fit)

# 6.6
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
#         start=c(2000,1),
#         frequency = 12)


#####################################
####             NOTES            ###
#####################################
# NIcE cOLUMN DROP EG:
# df <- subset( df, select = -c(x, y, z) )
# pay$obs <- c(1:174)
# pay <- subset
# m_exp_0014 <- xts(read.csv("expense_m_0007_1412_r174_c2.csv", header = F))

# FOR LATER AUTOMATION: 
# if(!require(data.table)){install.packages("data.table")}

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
