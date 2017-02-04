email_stats <- function(){
  # How to grab data from Netezza, eg: Calculate Daily Send and GM callout metrics
  # Remember that the .xls readers aren't connecting AO 3.8.16
  # CSV is a pain in the ass to export from Cognos and arrange properly
  library(nzr)
  library(dplyr)
  library(scales)
  library(lubridate) # not sure what we'd use this for, but...eg instead of Sys.Date() use today()...
  library(TTR)
  
  # connect to the the NZ DB - note the 4th param is the DATABASE you are accessing (table comes later)
  # One area of focus should be getting your own login and ensuring that IP doesn't change when you do
  
  #################################
  #################################
  #######      Get Data    ########
  #################################
  #################################
  #################################
  nzConnect("drew", "Kiopses1", "192.168.100.5", "client_tracfone", force=TRUE)
  # nzShowTables() # This shows you all the tables in the DB you selected above
  
  # Get Sales from the GM_Reportable Table, format new Date
  sales.nz <- nz.data.frame("GM_REPORTABLE")
  summary(sales.df <- as.data.frame(sales.nz))
  sales.df$Date <- as.Date(sales.df$EVENT_DATE_EST)
  sales.df$EVENT_DATE_EST <- NULL
  
  # make date vars
  report_day <- Sys.Date()-1
  sma_7day <- report_day - 7
  billed_day <- "2016-02-01"
  
  sales.mid <- arrange(sales.df, desc(Date))
  # mean(sales.df$SALES[sales.df$Date >= sma_7day & sales.df$Date <= report_day])
  
  # pivot into days with sums of metrics over all brands
  sales <- sales.mid %>% 
    group_by(Date) %>% 
    mutate(Sales = sum(SALES),
           Cost = sum(COST),
           Revenue = sum(REVENUE)
           # ,n = n() # in case you want to know how many rows are in each group
    )
  
  # select distinct rows from dplyr, above (there are n = 4 per day)
  sales <- distinct(select(sales, Date, Sales, Cost, Revenue)) 
  
  # fill in the discounted revenue numbers (BEFORE CALCULATING GM!) for days > billed_day (param)
  sales$Revenue <- ifelse(sales$Date >= billed_day, sales$Revenue*0.967, sales$Revenue)  
  
  # Make GM calcs
  sales$GM <- sales$Revenue-sales$Cost
  sales$GMPerc <- sales$GM/sales$Revenue
  
  # Imps & Clicks
  imps_clicks.nz <- nz.data.frame("DAILY_SEND_PUB_DATA")
  summary(imps_clicks.df <- as.data.frame(imps_clicks.nz))
  imps_clicks.df$Date <- as.Date(imps_clicks.df$EVENT_DATE)
  imps_clicks.df$EVENT_DATE <- NULL
  
  imps_clicks.df <- arrange(imps_clicks.df, desc(Date))
  
  imps_clicks <- imps_clicks.df %>% 
    group_by(Date) %>% 
    mutate(Clicks = sum(CLICKS),
           Imps = sum(IMPRESSIONS)) %>%
    select(Date, Clicks, Imps)
  
  # select distinct rows from dplyr, above (there are n = 4 per day)
  imps_clicks <- distinct(select(imps_clicks, Date, Clicks, Imps)) 
  
  # equivalent join v merge
  ics <- left_join(sales, imps_clicks, by = "Date")
  
  # reorder ics object for SMAs
  # ics <- arrange(ics, Date)
  
  # SMAs
  ics$Sales_7 <- SMA(ics$Sales, 7)
  ics$Sales_30 <- SMA(ics$Sales, 30)
  ics$Cost_7 <- SMA(ics$Cost, 7)
  ics$Cost_30 <- SMA(ics$Cost, 30)
  ics$Revenue_7 <- SMA(ics$Revenue, 7)
  ics$Revenue_30 <- SMA(ics$Revenue, 30)
  ics$GM_7 <- SMA(ics$GM, 7)
  ics$GM_30 <- SMA(ics$GM, 30)
  ics$GMPerc_7 <- SMA(ics$GM/ics$Revenue, 7)
  ics$GMPerc_30 <- SMA(ics$GM/ics$Revenue, 30)
  ics$Clicks_7 <- SMA(ics$Clicks, 7)
  ics$Clicks_30 <- SMA(ics$Clicks, 30)
  ics$Imps_7 <- SMA(ics$Imps, 7)
  ics$Imps_30 <- SMA(ics$Imps, 30)
  
  ics$Month <- month(ics$Date)
  ics$DOW <- weekdays(ics$Date)
  
  ics$CPA <- ics$Cost/ics$Sales
  ics$CPM <- ics$Cost/ics$Imps
  
  # point SMAs for email
  sma_sales <- round(mean(ics$Sales[1:7]))
  sma_imps <- round(mean(ics$Imps[1:7]))
  sma_clicks <- round(mean(ics$Clicks[1:7]))
  sma_revenue <- round(mean(ics$Revenue[1:7]), digits = 2)
  sma_cost <- round(mean(ics$Cost[1:7]), digits = 2)
  sma_gm <- round(mean(ics$GM[1:7]), digits = 2)
  # this one needs to be weighted!
  sma_gmPerc <- mean(ics$GM[1:7]/ics$Revenue[1:7])
  
  (output <- list(Sales_MA = sma_sales,
                  SalesAVG_MTD = round(mean(sales$Sales[month(sales$Date) == month(Sys.Date())])),
                  SalesAVG_LastMonth = round(mean(sales$Sales[month(sales$Date) == month(Sys.Date())-1])),
                  SalesAVG_TwoMonthsAgo = round(mean(sales$Sales[month(sales$Date) == month(Sys.Date())-2])),
                  Impressions_MA = paste(round(sma_imps/1000000, digits = 1),"M",sep=''),
                  Clicks_MA = paste(round(sma_clicks/1000, digits = 1),"K",sep=''),
                  Revenue_MA = paste("$",round(sma_revenue/1000, digits = 1),"K", sep = ''),
                  RevenueAVG_MTD = paste("$", 
                                         round(
                                           mean(sales$Revenue[month(sales$Date) == month(Sys.Date())])/1000,
                                           digits = 1), 
                                         "K", sep = ''),
                  RevenueAVG_LastMonth = paste("$", 
                                                round(
                                                  mean(sales$Revenue[month(sales$Date) == month(Sys.Date())-1])/1000,
                                                  digits = 1), 
                                                "K", sep = ''),
                  RevenueAVG_TwoMonthsAgo = paste("$", 
                                                  round(
                                                    mean(sales$Revenue[month(sales$Date) == month(Sys.Date())-2])/1000,
                                                    digits = 1), 
                                                  "K", sep = ''),
                  Cost_MA = paste("$",round(sma_cost/1000, digits = 1),"K", sep = ''),
                  CostAVG_MTD = paste("$",
                                      round(
                                        mean(sales$Cost[month(sales$Date) == month(Sys.Date())])/1000,
                                        digits = 1),
                                      "K", sep =''),
                  CostAVG_LastMonth = paste("$",
                                             round(
                                               mean(sales$Cost[month(sales$Date) == month(Sys.Date())-1])/1000,
                                               digits = 1),
                                             "K", sep = ''),
                  CostAVG_TwoMonthsAgo = paste("$",
                                               round(
                                                 mean(sales$Cost[month(sales$Date) == month(Sys.Date())-2])/1000,
                                                 digits = 1),
                                               "K", sep = ''),
                  GM_MA = paste("$",round(sma_gm/1000, digits = 1),"K", sep = ''),
                  GMAVG_MTD = paste("$",
                                    round(
                                      mean(sales$GM[month(sales$Date) == month(Sys.Date())])/1000,
                                      digits = 1),
                                    "K", sep = ''),
                  GMAVG_LastMonth = paste("$",
                                           round(
                                             mean(sales$GM[month(sales$Date) == month(Sys.Date())-1])/1000,
                                             digits = 1),
                                           "K", sep = ''),
                  GMAVG_TwoMonthsAgo = paste("$",
                                             round(
                                               mean(sales$GM[month(sales$Date) == month(Sys.Date())-2])/1000,
                                               digits = 1),
                                             "K", sep = ''),
                  GM_Percent_MA = percent(sma_gmPerc),
                  GMPercAVG_MTD =
                    percent(
                      (sum(sales$Revenue[month(sales$Date) == month(Sys.Date())])-
                      sum(sales$Cost[month(sales$Date) == month(Sys.Date())]))/
                      sum(sales$Revenue[month(sales$Date) == month(Sys.Date())])
                      ),
                  GMPercAVG_LastMonth = 
                    percent(
                    (sum(sales$Revenue[month(sales$Date) == month(Sys.Date())-1])-
                    sum(sales$Cost[month(sales$Date) == month(Sys.Date())-1]))/
                    sum(sales$Revenue[month(sales$Date) == month(Sys.Date())-1])
                      ),
                  GMPercAVG_TwoMonthsAgo = 
                    percent(
                      (sum(sales$Revenue[month(sales$Date) == month(Sys.Date())-2])-
                      sum(sales$Cost[month(sales$Date) == month(Sys.Date())-2]))/
                      sum(sales$Revenue[month(sales$Date) == month(Sys.Date())-2])
                    )
                  ))
  
  write.csv(t(as.data.frame(output)), "c:/Users/jvangeete/Desktop/report_email.csv", row.names = T)
  
  
  # ics$Sales_7_30_diff <- ics$Sales_7 - ics$Sales_30
  # ics$Date[ics$Sales_7_30_diff > 0]
}