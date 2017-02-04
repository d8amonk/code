# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# ///       ***FORECAST v2 - see v1 4 snips***       ///
# ///                                                ///
# ///                                                ///
# //////////////////////////////////////////////////////
# /////////////////////notes.../////////////////////////
# detach("package:xtable", unload=TRUE)
# knit_hooks
# grep("^render_", ls("package:knitr"), value = TRUE)

  
# /////////////////////////////////////
# ////       UPCOMING TASKS         ///
# /////////////////////////////////////

# AUTOMATE / FUNCTION CALLS < - function(data, t_hist_start, t_hist_end, T, f_start, f_end, ...para1, para2, para3)

# ////////////////////////////////////////////////
# ////        LOADING & BUILDING AREA          ///
# ////////////////////////////////////////////////

options(scipen=6, digits = 3)

library(dplyr, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(forecast, quietly = T, warn.conflicts = F)

# So data
setwd("c:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/") 
setwd("c:/Users/vangeetej/Google Drive/CDOT/Data/On Deck/") 

raw <- tbl_dt(read.csv("c:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/Expenditure_byPayment_00to14_wRegions.csv"))
#Build a temp_Date object using the date features of each contract
raw$Date <- as.Date(raw$Date, format="%m/%d/%Y")

# check for NAs (should return a "0-length row.names" object)
summary(raw[!complete.cases(raw),])

#Check before moving on...
glimpse(raw)

# /////////////////////////////////////
# ////     FILTER & TRAINING AREA   ///
# /////////////////////////////////////
# Using dplyr: build contract length, num payments, and total payments 
# (Replaces pivot sections)

exp <- raw %>% 
  group_by(ID) %>% 
  mutate(start = as.Date(min(Date)),
         start_year = year(start),
         start_month = month(start),
         end = as.Date(max(Date)),
         len_days = as.numeric(difftime(last(Date), first(Date), unit="days")),
         len_months = ceiling(len_days/30), 
         num_payments = n(),
         tot = sum(Amount)) %>%
  arrange(ID)

# DISTINCT CONTRACTS
distinct_contracts <- distinct(select(exp, ID, Region, Contractor, num_payments, start, start_year, start_month, end, len_months, tot))

# ...BY START Y/M 
exp_starts <- distinct_contracts %>%
  arrange(start_year, start_month, start)

# ...FILTERED > 2001
exp1 <- dplyr::filter(exp_starts, start_year >= 2001)

# PAYMENTS
r1 <- dplyr::filter(exp, Region == 1)
r2 <- dplyr::filter(exp, Region == 2)
r3 <- dplyr::filter(exp, Region == 3)
r4 <- dplyr::filter(exp, Region == 4)
r5 <- dplyr::filter(exp, Region == 5)

# CONTRACTS
r1c <- dplyr::filter(exp_starts, Region == 1)
r2c <- dplyr::filter(exp_starts, Region == 2)
r3c <- dplyr::filter(exp_starts, Region == 3)
r4c <- dplyr::filter(exp_starts, Region == 4)
r5c <- dplyr::filter(exp_starts, Region == 5)


# REGIONAL ANALYSIS
require(data.table)
require(ggplot2)
require(ggthemes)
require(gridExtra)
par(mfrow=c(2,1))

# REGION: ALL 
e_amt <- exp$Amount
e_ts <- ts(e_amt, start = c(2000,7), end = c(2014,12), frequency = 12)
e_dt <- data.table(e_ts) #forecast object, below
p <- plot(e_ts, main = "All Regions: (1) By All (+/-) Payments (2) By Contract Total", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

e_contracts <- exp_starts$tot
ec_ts <- ts(e_contracts, start = c(2000,7), end = c(2014,12), frequency = 12)
ec_dt <- data.table(ec_ts) #forecast object, below
q <- plot(ec_ts, xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

# REGION: 1 
r1_amt <- r1$Amount
r1_ts <- ts(r1_amt, start = c(2000,7), end = c(2014,12), frequency = 12)
r1_dt <- data.table(r1_ts) #forecast object, below
plot(r1_ts, main = "Region 1, All (+/-) Payments", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

r1_contracts <- r1c$tot
r1c_ts <- ts(r1_contracts, start = c(2000,7), end = c(2014,12), frequency = 12)
r1c_dt <- data.table(r1c_ts) #forecast object, below
plot(r1c_ts, main = "Region 1, By Contract Total", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

# REGION: 2 
r2_amt <- r2$Amount
r2_ts <- ts(r2_amt, start = c(2000,7), end = c(2014,12), frequency = 12)
r2_dt <- data.table(r2_ts) #forecast object, below
plot(r2_ts, main = "Region 2, All (+/-) Payments", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

r2_contracts <- r2c$tot
r2c_ts <- ts(r2_contracts, start = c(2000,7), end = c(2014,12), frequency = 12)
r2c_dt <- data.table(r2c_ts) #forecast object, below
plot(r2c_ts, main = "Region 2, By Contract Total", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

# REGION: 3 
r3_amt <- r3$Amount
r3_ts <- ts(r3_amt , start = c(2000,7), end = c(2014,12), frequency = 12)
r3_dt <- data.table(r3_ts) #forecast object, below
plot(r3_ts, main = "Region 3, All (+/-) Payments", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

r3_contracts <- r3c$tot
r3c_ts <- ts(r3_contracts , start = c(2000,7), end = c(2014,12), frequency = 12)
r3c_dt <- data.table(r3c_ts) #forecast object, below
plot(r3c_ts, main = "Region 3, By Contract Total", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

# REGION: 4 
r4_amt <- r4$Amount
r4_ts <- ts(r4_amt, start = c(2000,7), end = c(2014,12), frequency = 12)
r4_dt <- data.table(r4_ts) #forecast object, below
plot(r4_ts, main = "Region 4, All (+/-) Payments", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

r4_contracts <- r4c$tot
r4c_ts <- ts(r4_contracts, start = c(2000,7), end = c(2014,12), frequency = 12)
r4c_dt <- data.table(r4c_ts) #forecast object, below
plot(r4c_ts, main = "Region 4, By Contract Total", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

# REGION: 5
r5_amt <- r5$Amount
r5_ts <- ts(r5_amt, start = c(2000,7), end = c(2014,12), frequency = 12)
r5_dt <- data.table(r5_ts) #forecast object, below
plot(r5_ts, main = "Region 5, All (+/-) Payments", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

r5_contracts <- r5c$tot
r5c_ts <- ts(r5_contracts, start = c(2000,7), end = c(2014,12), frequency = 12)
r5c_dt <- data.table(r5c_ts) #forecast object, below
plot(r5c_ts, main = "Region 5, By Contract Total", xaxt = 'n', xlab = "Calendar Year", ylab = "$$$")
axis(1, at = seq(2000, 2015, by = 1), las=1)

# TOP CONTRACTOR
s1 <- summary(r1c$Contractor)[1]
d1 <- dim(r1c)[1]
# Alpha Concentration
scales::percent(s1/d1)

s2 <- summary(r2c$Contractor)[1]
d2 <- dim(r2c)[1]
# Alpha Concentration
scales::percent(s2/d2)

s3 <- summary(r3c$Contractor)[1]
d3 <- dim(r3c)[1]
# Alpha Concentration
scales::percent(s3/d3)

s4 <- summary(r4c$Contractor)[1]
d4 <- dim(r4c)[1]
# Alpha Concentration
scales::percent(s4/d4)

s5 <- summary(r5c$Contractor)[1]
d5 <- dim(r5c)[1]
# Alpha Concentration
scales::percent(s5/d5)

sA <- summary(exp_starts$Contractor)[1]
dA <- dim(exp_starts)[1]
# Alpha Concentration
scales::percent(sA/dA)


plot.ts(c(r1_ts, r2_ts, r3_ts, r4_ts,r5_ts), type = 'l')
plot(c(r1c_ts, r2c_ts, r3c_ts, r4c_ts,r5c_ts), type = 'l')

plot(r1_ts)
plot(r2_ts, update = F)


###NOTES:

require(ggplot2)
require(ggthemes)
# all regions
e_gg <- ggplot(exp, aes(x = Date, y = Amount, color = floor(start_year)))
e_gg + geom_jitter(aes(size = tot)) + theme_economist() + scale_color_continuous()
# region 1
r1_8  <- dplyr::filter(r1, year(Date) == 2008) # year = 2008
r1_gg <- ggplot(r1_8, aes(x = Date, y = Amount))
r1_gg + geom_point(aes(color = as.factor(year(Date)), size = tot)) + theme_calc() + scale_color_calc() + scale_size_area(max_size = 9) + 
  ggtitle("Payments in 2008, Colored by YEAR OF PAYMENT (size = CONTRACT TOTAL, the sum of all Payments, all Years)") # INCR. Encumbrance
r1_gg + geom_point(aes(color = as.factor(start_year), size = tot)) + theme_calc() + scale_color_calc() + scale_size_area(max_size = 9) + 
  ggtitle("Payments in 2008, Colored by CONTRACT START YEAR (size = CONTRACT TOTAL, the sum of all Payments, all Years)") # Current Encumbrance

r1_8on  <- dplyr::filter(r1, year(Date) >= 2008) # years since+incl 2008
r1_gg <- ggplot(r1_8on, aes(x = Date, y = Amount))
par(mfrow = c(2,1))
r1_gg + geom_point(aes(color = as.factor(year(Date)), size = tot)) + ylim(0,5500000) + theme_calc() + scale_color_calc() + scale_size_area(max_size = 6) +
  ggtitle("Payments (2008-2014) colored by YEAR OF PAYMENT ~ 'incremental encumbrance'") #INCR. Encumbrance
r1_gg + geom_point(aes(color = as.factor(start_year), size = tot)) + ylim(0,5500000)+ theme_calc() + scale_color_calc() + scale_size_area(max_size = 6) +
  ggtitle("Payments (2008-2014) colored by CONTRACT START YEAR ~ 'NON-incremental encumbrance'") #INCR. Encumbrance


# region 2
# r2_gg <- ggplot(r2, aes(x = Date, y = Amount))
r2_8  <- dplyr::filter(r2, start_year > 2007) # years after 2007

r2_gg #gg object

# region 3
# r3_gg <- ggplot(r3, aes(x = Date, y = Amount))
r3_8  <- dplyr::filter(r3, start_year > 2007) # years after 2007

r3_gg #gg object

# region 4
# r4_gg <- ggplot(r4, aes(x = Date, y = Amount))
r4_8  <- dplyr::filter(r4, start_year > 2007) # years after 2007

r4_gg #gg object

# region 5
# r5_gg <- ggplot(r5, aes(x = Date, y = Amount))
r5_8  <- dplyr::filter(r5, start_year > 2007) # years after 2007

r5_gg #gg object

# regions <- data.frame(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5)
# /////////////////////////////////////
# ////         FITTING AREA         ///
# ////                              ///
# ////              KEEP IT CLEAN!  ///
# /////////////////////////////////////

# fitting two linear models FOR DEMO
fit1 <- lm(tot ~ 0 + len_months, exp_starts)
fit2 <- lm(tot ~ 0 + num_payments, exp_starts)
fit3 <- lm(tot ~ 0 + start, exp_starts) #this should work out to be inflation (1.16%)
fit4 <- lm(len_months*30 ~ 0 + year(start), exp_starts)
fit5 <- lm(num_payments ~ 0 + year(start), exp_starts)
summary(fit1) #ea day of const. time is assoc w/ +$14,502.27
summary(fit2) #ea contract payment is assoc w/ +$610,379
summary(fit3) #TEST: calendar +days alone account for +$516 (3.6%) of fit1
summary(fit4) #contract lengths increased 0.28 days/year over 2000-2014 
summary(fit5) #significant zero effect

# Keepahs
plot.ts(ec_ts) #trend line is flat - !abline

plot(exp_starts$len_months, exp_starts$tot)
abline(fit1, col = 2, lwd = 2)
text(x = 2500, y = 50000000, "+$14,502/day", cex = 1.2, col = 2)

tot_len  <- ggplot(exp, aes(x = len, y = tot, z = Date))
tot_len + 
  geom_jitter(aes(color = as.factor(year(Date)), size = len_months, alpha = 0.8)) + 
  theme_gdocs() +
  scale_color_gdocs() +
  theme(legend.position = "none") + 
  ggtitle("Does Contract Length Drive Total Spend?")

# /////////////////////////////////////
# ////                              ///
# //// FORECASTING AREA             ///
# ////     PLOT SPARINGLY           ///
# ////                (USE_GFX!)    ///
# ////                              ///
# ////              KEEP IT CLEAN!  ///
# ////                -mgmt         ///
# ////                              ///
# /////////////////////////////////////
dev.off()
fit3 <- stl(r1c_ts, 
              s.window = 12, s.degree = 1,
              t.window = 12, t.degree = 1,
              l.window = 12, l.degree = 1,
              robust = T)
plot(fit3)
plot(forecast(fit3))

acf(r1c_ts, 
    lag.max = 36,
    plot = TRUE, 
    na.action = na.pass, 
    demean = TRUE, main = "DEMO ONLY")


# Adjust (non-ETS objects!) for seasonality
d_adj <- seasadj(fit3)
acf(d_adj)
plot(d_adj, main = "Annual CDOT Expenditures, Seasonally Adj.")

fit4 <- ets(r1c_ts)
x <- forecast(fit4, h = 24, 
              simulate = T, 
              bootstrap = T)
accuracy(x)
summary(x)
plot(x)

# Slice and serve
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