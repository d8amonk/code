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
ics <- arrange(ics, Date)

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

ics_save <- ics
#################################
#################################
#######       Plots      ########
#################################
#################################
#################################
library(ggplot2)
# all data (weeks and weekends) together
plot_sales <- ggplot(ics, aes(x = Date, y = Sales)) + geom_line()
plot_sales <- plot_sales + geom_line(aes(x = Date, Sales_7), color = 'Red') + geom_line(aes(x = Date, Sales_30), color = 'Blue')

plot_cost <- ggplot(ics, aes(x = Date, y = Cost)) + geom_line()
plot_cost <- plot_cost + geom_line(aes(x = Date, Cost_7), color = 'Red') + geom_line(aes(x = Date, Cost_30), color = 'Blue')

plot_rev <- ggplot(ics, aes(x = Date, y = Revenue)) + geom_line()
plot_rev <- plot_rev + geom_line(aes(x = Date, Revenue_7), color = 'Red') + geom_line(aes(x = Date, Revenue_30), color = 'Blue')

plot_GM <- ggplot(ics, aes(x = Date, y = GM)) + geom_line()
plot_GM <- plot_GM + geom_line(aes(x = Date, GM_7), color = 'Red') + geom_line(aes(x = Date, GM_30), color = 'Blue')

plot_GMPerc <- ggplot(ics, aes(x = Date, y = GMPerc)) + geom_line()
plot_GMPerc <- plot_GMPerc + geom_line(aes(x = Date, GMPerc_7), color = 'Red') + geom_line(aes(x = Date, GMPerc_30), color = 'Blue')

plot_clicks <- ggplot(ics, aes(x = Date, y = Clicks)) + geom_line()
plot_clicks <- plot_clicks + geom_line(aes(x = Date, Clicks_7), color = 'Red') + geom_line(aes(x = Date, Clicks_30), color = 'Blue')

plot_imps <- ggplot(ics, aes(x = Date, y = Imps)) + geom_line()
plot_imps <- plot_imps + geom_line(aes(x = Date, Imps_7), color = 'Red') + geom_line(aes(x = Date, Imps_30), color = 'Blue')

# just weekdays
plot_sales_week <- ggplot(ics_week, aes(x = Date, y = Sales)) + geom_line()
plot_sales_week <- plot_sales_week + geom_line(aes(x = Date, Sales_7), color = 'Red') + geom_line(aes(x = Date, Sales_30), color = 'Blue')

plot_cost_week <- ggplot(ics_week, aes(x = Date, y = Cost)) + geom_line()
plot_cost_week <- plot_cost_week + geom_line(aes(x = Date, Cost_7), color = 'Red') + geom_line(aes(x = Date, Cost_30), color = 'Blue')

plot_rev_week <- ggplot(ics_week, aes(x = Date, y = Revenue)) + geom_line()
plot_rev_week <- plot_rev_week + geom_line(aes(x = Date, Revenue_7), color = 'Red') + geom_line(aes(x = Date, Revenue_30), color = 'Blue')

plot_GM_week <- ggplot(ics_week, aes(x = Date, y = GM)) + geom_line()
plot_GM_week <- plot_GM_week + geom_line(aes(x = Date, GM_7), color = 'Red') + geom_line(aes(x = Date, GM_30), color = 'Blue')

plot_GMPerc_week <- ggplot(ics_week, aes(x = Date, y = GMPerc)) + geom_line()
plot_GMPerc_week <- plot_GMPerc_week + geom_line(aes(x = Date, GMPerc_7), color = 'Red') + geom_line(aes(x = Date, GMPerc_30), color = 'Blue')

plot_clicks_week <- ggplot(ics_week, aes(x = Date, y = Clicks)) + geom_line()
plot_clicks_week <- plot_clicks_week + geom_line(aes(x = Date, Clicks_7), color = 'Red') + geom_line(aes(x = Date, Clicks_30), color = 'Blue')

plot_imps_week <- ggplot(ics_week, aes(x = Date, y = Imps)) + geom_line()
plot_imps_week <- plot_imps_week + geom_line(aes(x = Date, Imps_7), color = 'Red') + geom_line(aes(x = Date, Imps_30), color = 'Blue')

# just weekends
plot_sales_weekend <- ggplot(ics_weekend, aes(x = Date, y = Sales)) + geom_line()
plot_sales_weekend <- plot_sales_weekend + geom_line(aes(x = Date, Sales_7), color = 'Red') + geom_line(aes(x = Date, Sales_30), color = 'Blue')

plot_cost_weekend <- ggplot(ics_weekend, aes(x = Date, y = Cost)) + geom_line()
plot_cost_weekend <- plot_cost_weekend + geom_line(aes(x = Date, Cost_7), color = 'Red') + geom_line(aes(x = Date, Cost_30), color = 'Blue')

plot_rev_weekend <- ggplot(ics_weekend, aes(x = Date, y = Revenue)) + geom_line()
plot_rev_weekend <- plot_rev_weekend + geom_line(aes(x = Date, Revenue_7), color = 'Red') + geom_line(aes(x = Date, Revenue_30), color = 'Blue')

plot_GM_weekend <- ggplot(ics_weekend, aes(x = Date, y = GM)) + geom_line()
plot_GM_weekend <- plot_GM_weekend + geom_line(aes(x = Date, GM_7), color = 'Red') + geom_line(aes(x = Date, GM_30), color = 'Blue')

plot_GMPerc_weekend <- ggplot(ics_weekend, aes(x = Date, y = GMPerc)) + geom_line()
plot_GMPerc_weekend <- plot_GMPerc_weekend + geom_line(aes(x = Date, GMPerc_7), color = 'Red') + geom_line(aes(x = Date, GMPerc_30), color = 'Blue')

plot_clicks_weekend <- ggplot(ics_weekend, aes(x = Date, y = Clicks)) + geom_line()
plot_clicks_weekend <- plot_clicks_weekend + geom_line(aes(x = Date, Clicks_7), color = 'Red') + geom_line(aes(x = Date, Clicks_30), color = 'Blue')

plot_imps_weekend <- ggplot(ics_weekend, aes(x = Date, y = Imps)) + geom_line()
plot_imps_weekend <- plot_imps_weekend + geom_line(aes(x = Date, Imps_7), color = 'Red') + geom_line(aes(x = Date, Imps_30), color = 'Blue')

# point SMAs for email
sma_sales <- round(mean(sales$Sales[1:7]))
sma_imps <- round(mean(ics$Imps[1:7]))
sma_clicks <- round(mean(ics$Clicks[1:7]))
sma_revenue <- round(mean(sales$Revenue[1:7]), digits = 2)
sma_cost <- round(mean(sales$Cost[1:7]), digits = 2)
sma_gm <- round(mean(sales$GM[1:7]), digits = 2)
# this one needs to be weighted!
sma_gmPerc <- mean(sales$GM[1:7]/sales$Revenue[1:7])

(output <- list(Sales_MA = sma_sales,
                Impressions_MA = paste(round(sma_imps/1000000, digits = 1),"M",sep=''),
                Clicks_MA = paste(round(sma_clicks/1000, digits = 1),"K",sep=''),
                Revenue_MA = paste("$",round(sma_revenue/1000, digits = 1),"K", sep = ''),
                Cost_MA = paste("$",round(sma_cost/1000, digits = 1),"K", sep = ''),
                GM_MA = paste("$",round(sma_gm/1000, digits = 1),"K", sep = ''),
                GM_Percent_MA = percent(sma_gmPerc)))

# ics$Sales_7_30_diff <- ics$Sales_7 - ics$Sales_30
# ics$Date[ics$Sales_7_30_diff > 0]

#################################
#################################
#######  Sales Crosses   ########
#################################
#################################
#################################
# sales cross up
ics$Sales.cross.up[1:30] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] > ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] < ics$Sales_30[(i - 1)])) {
    ics$Sales.cross.up[i] <- 1
  }
  else {
    ics$Sales.cross.up[i] <- 0
  }
}
sum(ics$Sales.cross.up, na.rm = T)
Sales.cross.up <- filter(ics, Sales.cross.up == 1)

# sales cross down
ics$Sales.cross.down[1:30] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] < ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] > ics$Sales_30[(i - 1)])) {
    ics$Sales.cross.down[i] <- 1
  }
  else {
    ics$Sales.cross.down[i] <- 0
  }
}
sum(ics$Sales.cross.down, na.rm = T)
Sales.cross.down <- filter(ics, Sales.cross.down == 1)

# cost cross up
ics$Cost.cross.up[1:30] <- NA
for (i in 31:length(ics$Cost)) {
  if ((ics$Cost_7[i] > ics$Cost_30[i]) & (ics$Cost_7[(i - 1)] < ics$Cost_30[(i - 1)])) {
    ics$Cost.cross.up[i] <- 1
  }
  else {
    ics$Cost.cross.up[i] <- 0
  }
}
sum(ics$Cost.cross.up, na.rm = T)
filter(ics, Cost.cross.up == 1)

# cost cross down
ics$Cost.cross.down[1:30] <- NA
for (i in 31:length(ics$Cost)) {
  if ((ics$Sales_7[i] < ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] > ics$Sales_30[(i - 1)])) {
    ics$SMA.cross.down[i] <- 1
  }
  else {
    ics$SMA.cross.down[i] <- 0
  }
}
sum(ics$SMA.cross.down, na.rm = T)
filter(ics, SMA.cross.down == 1)
# Clicks cross up
ics$SMA.cross.up[1:30] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] > ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] < ics$Sales_30[(i - 1)])) {
    ics$SMA.cross.up[i] <- 1
  }
  else {
    ics$SMA.cross.up[i] <- 0
  }
}
sum(ics$SMA.cross.up, na.rm = T)
filter(ics, SMA.cross.up == 1)
# Clicks cross down
ics$SMA.cross.down[1:30] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] < ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] > ics$Sales_30[(i - 1)])) {
    ics$SMA.cross.down[i] <- 1
  }
  else {
    ics$SMA.cross.down[i] <- 0
  }
}
sum(ics$SMA.cross.down, na.rm = T)
filter(ics, SMA.cross.down == 1)
# Imps cross up
ics$SMA.cross.up[1:30] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] > ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] < ics$Sales_30[(i - 1)])) {
    ics$SMA.cross.up[i] <- 1
  }
  else {
    ics$SMA.cross.up[i] <- 0
  }
}
sum(ics$SMA.cross.up, na.rm = T)
filter(ics, SMA.cross.up == 1)
# Imps cross down
ics$SMA.cross.down[1:30] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] < ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] > ics$Sales_30[(i - 1)])) {
    ics$SMA.cross.down[i] <- 1
  }
  else {
    ics$SMA.cross.down[i] <- 0
  }
}
sum(ics$SMA.cross.down, na.rm = T)
filter(ics, SMA.cross.down == 1)
