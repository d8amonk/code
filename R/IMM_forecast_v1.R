# /////////////////////////////////////
# ////                              ///
# //// FORECASTING AREA             ///
# ////     PLOT SPARINGLY           ///
# ////                (USE GFX!)    ///
# ////                              ///
# ////              KEEP IT CLEAN!  ///
# ////                -mgmt         ///
# ////                              ///
# /////////////////////////////////////

# so library
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("dplyr", "ggplot2", "ggthemes", "scales", "fpp"))
options(scipen=999)

# such data
setwd("Z:\\DA&DS\\Jeff\\Data")
ds <- read.csv("ds_fc_trial.csv")

# convert the date column
ds$Day <- as.Date(ds$Day, format = "%m/%d/%Y")

sales <- as.ts(data.frame(Date = ds$Day, Sales = ds$Total.Sales))

  
    
# run'em
# xxxxx <- stl(ds$Total.Sales, 
#               s.window = 12, s.degree = 1,
#               t.window = 12, t.degree = 1,
#               l.window = 12, l.degree = 1,
#               robust = T)
# plot(xxxxx)
# plot(forecast(xxxxx))
# 
# acf(xxxxx, 
#     lag.max = 36,
#     plot = TRUE, 
#     na.action = na.pass, 
#     demean = TRUE, main = "DEMO ONLY")

# # Adjust for seasonality
# d_adj <- seasadj(sales)
# acf(d_adj)
# plot(xxxxx)
# plot(d_adj, main = "Annual Expenditures, Seasonally Adj.") # eg

# SUBSAMPLE DATES
# e_ts <- e_ts %>% filter(Date > as.Date("2010-01-01"))

# ETS IS USED TO FIND THE s,t,l PARAMS USED 
# ABOVE TO BRING THE MODEL TO STAIONARITY
fc1 <- ets(sales)

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

