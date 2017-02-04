# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# ///       ***FORECAST v2 - see v1 4 snips***       ///
# ///                                                ///
# ///                                                ///
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////

# /////////////////////////////////////
# ////       UPCOMING TASKS         ///
# /////////////////////////////////////

# AUTOMATE / FUNCTION CALLS < - function(data, t_hist_start, t_hist_end, T, f_start, f_end, ...para1, para2, para3)

# ////////////////////////////////////////////////
# ////        LOADING & BUILDING AREA          ///
# ////////////////////////////////////////////////

# Much libraries
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("dplyr", "ggplot2", "ggthemes", scales))
options(scipen=999)

# So data
setwd("c:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/") 
setwd("c:/Users/vangeetej/Google Drive/CDOT/Data/On Deck/") 

e <- tbl_dt(read.csv("Expenditure_byPayment_00to14_wRegions.csv"))

#Build a temp_Date object using the date features of each contract
e$Date <- as.Date(e$Date, format="%m/%d/%Y")

# check for NAs (should return a "0-length row.names" object)
summary(e[!complete.cases(e),])

#Check before moving on...
glimpse(e)
 
# /////////////////////////////////////
# ////     FILTER & TRAINING AREA   ///
# /////////////////////////////////////
# Using dplyr: build contract length, num payments, and total payments 
# (Replaces pivot sections)

exp <- 
  e %>% 
  group_by(ID) %>% 
  mutate(start = as.Date(min(Date)),
         start_year = year(start),
         start_month = month(start),
         end = as.Date(max(Date)),
         len = as.numeric(difftime(last(Date), first(Date), unit="days")),
         len_months = ceiling(len/30), 
         num_payments = n(),
         tot = sum(Amount),
         pays = slice(Amount > 0),
         adjs = num_payments - pays)

#Average Length between the payments for Dan!!!!!

exp  <- distinct(select(exp, 
                        ID,
                        Region,
                        Contractor,
                        start_year,
                        start_month,
                        start,
                        end,
                        len,
                        len_months,
                        num_payments,
                        tot)) %>%
  arrange(start)

#Realigns all payments to 'YYYY-MM-01' (ISO)
byMonth <- exp %>% 
  group_by(start_year, start_month) %>%
  summarise(total = tot,
            date = as.Date(paste(start_year, start_month, "01", sep="-")),
            Region = Region)
  
region1 <- dplyr::filter(exp, Region == 1)
region2 <- dplyr::filter(exp, Region == 2)
region3 <- dplyr::filter(exp, Region == 3)
region4 <- dplyr::filter(exp, Region == 4)
region5 <- dplyr::filter(exp, Region == 5)

region1_byMonth <- dplyr::filter(byMonth, Region == 1)
region2_byMonth <- dplyr::filter(byMonth, Region == 2)
region3_byMonth <- dplyr::filter(byMonth, Region == 3)
region4_byMonth <- dplyr::filter(byMonth, Region == 4)
region5_byMonth <- dplyr::filter(byMonth, Region == 5)

e_tot <- exp$tot
e_ts <- ts(e_tot, start = c(2001,1), end = c(2014,12), frequency = 12)
e_dt <- data.table(e_ts)
plot(e_dt, lwd = 2, ylab = "$$$")
plot
plot(byMonth$Date, byMonth$Total, 
     type = 'l', 
     xlab = "Date", 
     ylab = "Amount", 
     main = "CDOT Monthly Expenditures, 2001-2014")

r1_amt <- region1$Amount
r1_ts <- ts(r1_amt, start = c(2001,1), end = c(2014,12), frequency = 12)
r1_dt <- data.table(r1_ts)
r1 <- ggplot(region1, aes(x = Date, y = Amount)) + 
  geom_density(aes(y = ..density..))
r1

r2_amt <- region2$Amount
r2_ts <- ts(r2_amt, start = c(2001,1), end = c(2014,12), frequency = 12)
r2_dt <- data.table(r2_ts)
r2 <- ggplot(region2, aes(x = Date, y = Amount)) + 
  geom_density(aes(y = ..density..))
r2

r3_amt <- region3$Amount
r3_ts <- ts(r3_amt, start = c(2001,1), end = c(2014,12), frequency = 12)
r3_dt <- data.table(r3_ts)
r3 <- ggplot(region3, aes(x = Date, y = Amount)) + 
  geom_density(aes(y = ..density..))
r3

r4_amt <- region4$Amount
r4_ts <- ts(r4_amt, start = c(2001,1), end = c(2014,12), frequency = 12)
r4_dt <- data.table(r4_ts)
r4 <- ggplot(region4, aes(x = Date, y = Amount)) + 
  geom_density(aes(y = ..density..))
r4

r5_amt <- region5$Amount
r5_ts <- ts(r5_amt, start = c(2001,1), end = c(2014,12), frequency = 12)
r5_dt <- data.table(r5_ts)
r5 <- ggplot(region5, aes(x = Date, y = Amount)) + 
  geom_density(aes(y = ..density..))
r5

regions <- data.frame(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5)
# /////////////////////////////////////
# ////         FITTING AREA         ///
# ////                              ///
# ////              KEEP IT CLEAN!  ///
# /////////////////////////////////////

# fitting two linear models FOR DEMO
fit1 <- lm(tot ~ 0 + len, exp)
fit2 <- lm(tot ~ 0 + num_payments, exp)
fit3 <- lm(tot ~ 0 + Date, exp)
fit4 <- lm(len ~ 0 + year(Date), exp)
fit5 <- lm(num_payments ~ 0 + year(Date), exp)
summary(fit1) #ea day of const. time is assoc w/ +$14,502.27
summary(fit2) #ea contract payment is assoc w/ +$610,379
summary(fit3) #TEST: calendar +days alone account for +$516 (3.6%) of fit1
summary(fit4) #contract lengths increased 0.28 days/year over 2000-2014 
summary(fit5) #significant zero effect

# Keepahs
plot.ts(e_ts) #trend line is flat - !abline

plot(exp$len, exp$tot)
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

fit3 <- stl(region1, 
              s.window = 12, s.degree = 1,
              t.window = 12, t.degree = 1,
              l.window = 12, l.degree = 1,
              robust = T)
plot(fit3)
plot(forecast(fit3))

acf(e_ts, 
    lag.max = 36,
    plot = TRUE, 
    na.action = na.pass, 
    demean = TRUE, main = "DEMO ONLY")
  
# LEAVE UP FOR DEMO
fit7 <- stl(e_ts,
           s.window = 12, s.degree = 1,
           t.window = 3, t.degree = 1,
           l.window = 3, l.degree = 1,
           robust = T)
# ETS IS USED TO FIND THE s,t,l PARAMS USED 
# ABOVE TO BRING THE MODEL TO STAIONARITY

# Adjust for seasonality
d_adj <- seasadj(fit7)
acf(d_adj)
plot(fit7)
plot(d_adj, main = "Annual CDOT Expenditures, Seasonally Adj.")

# SUBSAMPLE DATES
# e_ts <- e_ts %>% filter(Date > as.Date("2010-01-01"))

fit9 <- ets(fit7)

summary(fit9)
x <- forecast(fit7, h = 24) 
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

