//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
///       ***this is the FORECAST section***       ///
///         this section runs on SM_, H_           ///
///         and _M named, cleaned objects          ///
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////

/////////////////////////////////////
////       UPCOMING TASKS         ///
/////////////////////////////////////

# AUTOMATE / FUNCTION CALLS < - function(data, t_hist_start, t_hist_end, T, f_start, f_end, ...para1, para2, para3)

////////////////////////////////////////////////
////        LOADING & BUILDING AREA          ///
////////////////////////////////////////////////

# Much libraries
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("dplyr","data.table"))

# So data
setwd("c:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/") 

# setwd("c:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/") 

exp.s <- tbl_dt(read.csv("2001_2014_ExpByContr_Cleaned.csv"))

options(scipen=999)

# test <- data.frame("c:/Users/vangeetej/Google Drive/CDOT/Data/Jason Pulls/Query3_v2.csv", header = T)

h_01_14 <- read.csv("SM_01_14.csv")

# str(h_01_14)
# 
# m_01_14 <- read.csv("m_01_14.csv", F)
m_01_16 <- read.csv("m_01_16.csv", F)
# m_10_16 <- read.csv("m_10_16.csv", F)
# 
# colnames(m_01_14) <- c("Year", "Month", "Count", "Amount")
# colnames(m_01_16) <- c("Date", "Amount")
# colnames(m_10_16) <- c("Date", "Amount")

Build a temp_Date object using the date features of each contract
temp_Date <- as.Date(paste(h_01_14$Month,h_01_14$Day,h_01_14$Year,sep="/"), format="%m/%d/%Y")

# temp_Date_01_14 <- seq(as.Date("2001-01-01"), as.Date("2014-12-31"), "months")
# temp_Date_01_16 <- seq(as.Date("2001-01-01"), as.Date("2016-12-31"), "months")
# temp_Date_10_16 <- seq(as.Date("2010-01-01"), as.Date("2016-12-31"), "months")

# temp_Date_10 <- as.Date(paste(h_10_14$Month,h_10_14$Day,h_10_14$Year,sep="/"), format="%m/%d/%Y")
h_01_14 <- data.frame(h_01_14, Date = temp_Date)

# m_01_14 <- data.frame(m_01_14, Date = temp_Date_01_14)
# m_01_16 <- data.frame(h_01_14, Date = temp_Date_01_16)
# m_10_16 <- data.frame(m_10_16, Date = temp_Date_10_16)

# check for NAs (should return a "0-length row.names" object)
summary(h_01_14[!complete.cases(h_01_14),])

# make a data.frame with an ISO date column
# m_01_14 <- data.frame(m_01_14)
# m_01_16 <- data.frame(m_01_16)
# m_10_16 <- data.frame(m_10_16)
# m_01_14$Date <- temp_Date_01_14
# m_01_16$Date <- temp_Date_01_16
# m_10_16$Date <- temp_Date_10_16

#LOOK at the data
glimpse(h_01_14)

/////////////////////////////////////
////     FILTER & TRAINING AREA   ///
/////////////////////////////////////

# Using dplyr, build contract length, num payments, and total payments 
# (Replaces pivot sections)

exp <- h_01_14 %>% 
  group_by(ID) %>% 
  mutate(start = as.Date(min(Date)),
            end = as.Date(max(Date)),
            len = as.numeric(difftime(last(Date), first(Date), unit="days")),
            len_months = ceiling(len/30), 
            num_payments = n(),
            tot = sum(Amount)) %>% 
  arrange(start)

#FILTER FOR ZJ
exp.z <- zj_40 %>% 
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

libraries(c("fpp", "forecast", "timeSeries", "tseries", "xts"))
m_amt <- h_01_14$Amount
m_ts <- ts(m_amt, start = c(2001,1), end = c(2014,12), frequency = 12)
m_dt <- data.table(m_ts)

/////////////////////////////////////
////         FITTING AREA         ///
////     PLOTS FOR CHEX ONLY!     ///
////              KEEP IT CLEAN!  ///
/////////////////////////////////////

#fit <- tbats(exp$Amount, use.trend= TRUE, use.box.cox = TRUE, seasonal.periods = c(12))

# fitting two linear models FOR DEMO
fit1 <- lm(len ~ c_tot, exp.c)
fit2 <- lm(num_payments ~ c_tot, exp.c)

# comparison of plots with PRFs overlaid


fit3 <- stl(m_ts, 
           s.window = 12, s.degree = 1,
           t.window = 12, t.degree = 1,
           l.window = 12, l.degree = 1,
           robust = T)
plot(fit3, main = "DEMO ONLY")

acf(m_ts, 
    lag.max = 36,
    plot = TRUE, 
    na.action = na.pass, 
    demean = TRUE, main = "DEMO ONLY")

/////////////////////////////////////
////                              ///
//// FORECASTING AREA             ///
////     PLOT SPARINGLY           ///
////                (USE_GFX!)    ///
////                              ///
////              KEEP IT CLEAN!  ///
////                -mgmt         ///
////                              ///
/////////////////////////////////////

# LEAVE UP FOR DEMO
fit7 <- stl(m_ts,
           s.window = 12, s.degree = 1,
           t.window = 3, t.degree = 1,
           l.window = 3, l.degree = 1,
           robust = T)

# ETS IS USED TO FIND THE s,t,l PARAMS USED 
# ABOVE TO BRING THE MODEL TO STAIONARITY

ets(m_ts)

#Multiplicative error, additive damped trend, 
# Add seasadj()
d_adj <- seasadj(fit7)
acf(d_adj)
plot(fit7)
plot(d_adj, main = "Annual CDOT Expenditures, Seasonally Adj.")
# plot(naive(d_adj), ylab="Expenditure Index", main="Naive Forecasts of Seasonally Adjusted Data")

#SUBSAMPLE DATES
m_ts <- m_ts %>% filter(Date > as.Date("2010-01-01"))

fit9 <- ets(m_ts)
#             model = "MAM")
summary(fit9)
x <- forecast(fit9, h = 24, 
              simulate = F, 
              bootstrap = F)
accuracy(fit9)
summary(x)
plot(x)

#SLICE AND I.C.E
C.fcast <- data.table(Date = seq(as.Date("2017-01-01"), as.Date("2018-12-31"), "months"),
                      point_est = x$mean,
                      U80 = x$fitted[,1], 
                      U95 = x$upper[,2],
                      L80 = x$lower[,1], 
                      L95 = x$lower[,2])
View(x)
plot(x)

C.summary <- C.fcast %>% 
  dplyr::summarise(FY15_H95 = sum(U95[1:12]),
                   FY15_H80 = sum(U80[1:12]),
                   FY15_point = sum(point_est[1:12]),
                   FY15_L80 = sum(L80[1:12]),
                   FY15_L95 = sum(L95[1:12]),
                   FY16_H95 = sum(U95[13:24]), 
                   FY16_H80 = sum(U80[13:24]),
                   FY16_point = sum(point_est[13:24]),
                   FY16_L80 = sum(L80[13:24]),
                   FY16_L95 = sum(L95[13:24])
                   )

