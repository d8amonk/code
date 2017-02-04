# How to grab data from Netezza, eg: Calculate Daily Send and GM callout metrics
# Remember that the .xls readers aren't connecting AO 3.8.16
# CSV is a pain in the ass to export from Cognos and arrange properly
library(nzr)
library(dplyr)
library(scales)
library(lubridate) # not sure what we'd use this for, but...eg instead of Sys.Date() use today()...
library(TTR)
library(ggthemes)
library(ggplot2)
library(stringr)


# connect to the the NZ DB - note the 4th param is the DATABASE you are accessing (table comes later)
# One area of focus should be getting your own login and ensuring that IP doesn't change when you do

#######      Get Data    ########
nzConnect("drew", "Kiopses1", "192.168.100.5", "client_tracfone", force=TRUE)
# nzShowTables() # This shows you all the tables in the DB you selected above

# Get Sales from the GM_Reportable Table, format new Date
sales.nz <- nz.data.frame("DAILY_SEND_ACTUALIZED_VW")
summary(sales.df <- as.data.frame(sales.nz))
sales.df$Date <- as.Date(sales.df$SALE_DATE)
sales.df$BRAND <- str_trim(sales.df$SKU_BRAND, "both")
sales.df$BRAND <- as.factor(sales.df$BRAND)
sales.df$SALES <- sales.df$BILLABLE_QUANTITY
sales.df <- filter(sales.df, BRAND != "TelCel")

# make date vars
# report_day <- Sys.Date()-1
# sma_7day <- report_day - 7
# billed_day <- "2016-02-01"

sales.mid <- arrange(sales.df, desc(Date))
# mean(sales.df$SALES[sales.df$Date >= sma_7day & sales.df$Date <= report_day])

# pivot into days with sums of metrics over all brands
sales <- sales.mid %>% 
  group_by(Date) %>% 
  mutate(Sales = sum(SALES))
sales <- distinct(select(sales, Date, Sales)) 

sales_bybrand <- sales.mid %>% 
  group_by(Date, BRAND) %>% 
  mutate(Sales = sum(SALES))
sales_bybrand <- distinct(select(sales_bybrand, Date, Brand = BRAND, Sales)) 

# Imps & Clicks
imps_clicks.nz <- nz.data.frame("DAILY_SEND_PUB_DATA")
summary(imps_clicks.df <- as.data.frame(imps_clicks.nz))
imps_clicks.df$Date <- as.Date(imps_clicks.df$EVENT_DATE)
imps_clicks.df$BRAND  <- as.factor(imps_clicks.df$BRAND)
imps_clicks.df$EVENT_DATE <- NULL
imps_clicks.df$DEVICE <- NULL

# imps_clicks.df <- arrange(imps_clicks.df, desc(Date))

imps_clicks <- imps_clicks.df %>% 
  group_by(Date) %>% 
  mutate(Clicks = sum(CLICKS),
         Imps = sum(IMPRESSIONS)) %>%
  ungroup() %>% 
  arrange(desc(Date))
imps_clicks <- distinct(select(imps_clicks, Date, Clicks, Imps))

imps_clicks_bybrand <- imps_clicks.df %>% 
  group_by(Date, BRAND) %>% 
  mutate(Clicks = sum(CLICKS),
         Imps = sum(IMPRESSIONS)) %>%
  ungroup() %>% 
  arrange(desc(Date))
imps_clicks_bybrand <- distinct(select(imps_clicks_bybrand, Date, Brand = BRAND, Clicks, Imps))

# Prep factor strings for merge
sales_bybrand$Brand <- as.factor(str_trim(sales_bybrand$Brand, "both"))
imps_clicks_bybrand$Brand <- as.factor(str_trim(imps_clicks_bybrand$Brand, "both"))

# SALES JOIN IMPS_CLICKS
ics <- left_join(sales, imps_clicks, by = "Date")
ics_bybrand <- left_join(sales_bybrand, imps_clicks_bybrand, by = c("Date" = "Date", "Brand" = "Brand"))

# reorder ics object for SMAs
ics <- arrange(ics, Date)
ics_bybrand <- arrange(ics_bybrand, Date)
ics <- filter(ics, Date != Sys.Date())
ics_bybrand <- filter(ics_bybrand, Date != Sys.Date())

# 7, 30 day SMAs totals
ics$Sales_7 <- SMA(ics$Sales, 7)
ics$Sales_30 <- SMA(ics$Sales, 30)
ics$Clicks_7 <- SMA(ics$Clicks, 7)
ics$Clicks_30 <- SMA(ics$Clicks, 30)
ics$Imps_7 <- SMA(ics$Imps, 7)
ics$Imps_30 <- SMA(ics$Imps, 30)

ics$Month <- month(ics$Date)
ics$DOW <- weekdays(ics$Date)

# ics$CPA <- ics$Cost/ics$Sales
# ics$CPM <- ics$Cost/ics$Imps

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekends <- c("Saturday", "Sunday")

ics_weekend <- subset(ics, ics$DOW %in% weekends)
ics_week <- subset(ics, ics$DOW %in% weekdays)

ics_save <- ics

##Brand SMAs

ics_bybrand <- arrange(ics_bybrand, Date)
ics_bybrand <- ics_bybrand %>% 
      group_by(Brand) %>%
      mutate(
        Sales_7 = SMA(Sales, 7),
        Sales_30 = SMA(Sales, 30),
        Clicks_7 = SMA(Clicks, 7),
        Clicks_30 = SMA(Clicks, 30),
        Imps_7 = SMA(Imps, 7),
        Imps_30 = SMA(Imps, 30))  

#######       Plots

# Notes: par(mfrow = c(x,x)) and place weeks next to weekends - AK

#######       7-30MAs, full weeks
#######        all data (weeks and weekends) together ####
plot_sales <- ggplot(ics, aes(x = Date, y = Sales)) + geom_line()
plot_sales <- plot_sales + geom_line(aes(x = Date, Sales_7), color = 'Red') + geom_line(aes(x = Date, Sales_30), color = 'Blue')

plot_clicks <- ggplot(ics, aes(x = Date, y = Clicks)) + geom_line()
plot_clicks <- plot_clicks + geom_line(aes(x = Date, Clicks_7), color = 'Red') + geom_line(aes(x = Date, Clicks_30), color = 'Blue')

plot_imps <- ggplot(ics, aes(x = Date, y = Imps)) + geom_line()
plot_imps <- plot_imps + geom_line(aes(x = Date, Imps_7), color = 'Red') + geom_line(aes(x = Date, Imps_30), color = 'Blue')

#######        just weekdays  ####
plot_sales_week <- ggplot(ics_week, aes(x = Date, y = Sales)) + geom_line()
plot_sales_week <- plot_sales_week + geom_line(aes(x = Date, Sales_7), color = 'Red') + geom_line(aes(x = Date, Sales_30), color = 'Blue')

plot_clicks_week <- ggplot(ics_week, aes(x = Date, y = Clicks)) + geom_line()
plot_clicks_week <- plot_clicks_week + geom_line(aes(x = Date, Clicks_7), color = 'Red') + geom_line(aes(x = Date, Clicks_30), color = 'Blue')

plot_imps_week <- ggplot(ics_week, aes(x = Date, y = Imps)) + geom_line()
plot_imps_week <- plot_imps_week + geom_line(aes(x = Date, Imps_7), color = 'Red') + geom_line(aes(x = Date, Imps_30), color = 'Blue')

#######        just weekends  ####
plot_sales_weekend <- ggplot(ics_weekend, aes(x = Date, y = Sales)) + geom_line()
plot_sales_weekend <- plot_sales_weekend + geom_line(aes(x = Date, Sales_7), color = 'Red') + geom_line(aes(x = Date, Sales_30), color = 'Blue')

plot_clicks_weekend <- ggplot(ics_weekend, aes(x = Date, y = Clicks)) + geom_line()
plot_clicks_weekend <- plot_clicks_weekend + geom_line(aes(x = Date, Clicks_7), color = 'Red') + geom_line(aes(x = Date, Clicks_30), color = 'Blue')

plot_imps_weekend <- ggplot(ics_weekend, aes(x = Date, y = Imps)) + geom_line()
plot_imps_weekend <- plot_imps_weekend + geom_line(aes(x = Date, Imps_7), color = 'Red') + geom_line(aes(x = Date, Imps_30), color = 'Blue')

#######       by brand graphs ####
plot_sales_TW <- ggplot(subset(ics_bybrand, Brand == "Total Wireless")) + 
  geom_line(aes(Date, Sales, color = "Total Sales"))
(plot_sales_TW <- plot_sales_TW + 
  geom_line(aes(Date, Sales_7, color = "7-SMA"), lwd = 1.1) + 
  geom_line(aes(Date, Sales_30, color = "30-SMA"), lwd = 1.1) +
  theme_calc() + scale_color_manual(values = c("blue","red","grey")) +
  guides(color=guide_legend(title="")) + ggtitle("Total Wireless"))

plot_sales_ST <- ggplot(subset(ics_bybrand, Brand == "Straight Talk")) + 
  geom_line(aes(Date, Sales, color = "Total Sales"))
(plot_sales_ST <- plot_sales_ST + 
  geom_line(aes(Date, Sales_7, color = "7-SMA"), lwd = 1.1) + 
  geom_line(aes(Date, Sales_30, color = "30-SMA"), lwd = 1.1) +
  theme_calc() + scale_color_manual(values = c("blue","red","grey")) +
  guides(color=guide_legend(title="")) + ggtitle("Straight Talk")) 

plot_sales_NT <- ggplot(subset(ics_bybrand, Brand == "Net10")) + 
  geom_line(aes(Date, Sales, color = "Total Sales"))
(plot_sales_NT <- plot_sales_NT + 
  geom_line(aes(Date, Sales_7, color = "7-SMA"), lwd = 1.1) + 
  geom_line(aes(Date, Sales_30, color = "30-SMA"), lwd = 1.1) +
  theme_calc() + scale_color_manual(values = c("blue","red","grey")) +
  guides(color=guide_legend(title="")) + ggtitle("Net 10")) 

plot_sales_TF <- ggplot(subset(ics_bybrand, Brand == "Tracfone")) + 
  geom_line(aes(Date, Sales, color = "Total Sales"))
(plot_sales_TF <- plot_sales_TF + 
  geom_line(aes(Date, Sales_7, color = "7-SMA"), lwd = 1.1) + 
  geom_line(aes(Date, Sales_30, color = "30-SMA"), lwd = 1.1) +
  theme_calc() + scale_color_manual(values = c("blue","red","grey")) +
  guides(color=guide_legend(title="")) + ggtitle("Tracfone"))

#######  Sales Crosses
#######  Total Sales Crosses ####

# 7-30 sales cross up #QAd
ics$Sales.cross.up[1:length(ics$Sales)] <- NA
for (i in 31:length(ics$Sales)) {
  if ((ics$Sales_7[i] > ics$Sales_30[i]) & (ics$Sales_7[(i - 1)] < ics$Sales_30[(i - 1)])) {
    ics$Sales.cross.up[i] <- 1
  }
  else {
    ics$Sales.cross.up[i] <- 0
  }
}
sum(ics$Sales.cross.up, na.rm = T)
(Sales.cross.up <- filter(ics, Sales.cross.up == 1))

# 7-30 sales cross down #QAd
ics$Sales.cross.down[1:length(ics$Sales)] <- NA
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

# 7-30 Clicks cross up #QAd
ics$Clicks.cross.up[1:length(ics$Clicks)] <- NA
for (i in 31:length(ics$Clicks)) {
  if ((ics$Clicks_7[i] > ics$Clicks_30[i]) & (ics$Clicks_7[(i - 1)] < ics$Clicks_30[(i - 1)])) {
    ics$Clicks.cross.up[i] <- 1
  }
  else {
    ics$Clicks.cross.up[i] <- 0
  }
}
sum(ics$Clicks.cross.up, na.rm = T)
Clicks.cross.up <- filter(ics, Clicks.cross.up == 1)

# 7-30 Clicks cross down #QAd
ics$Clicks.cross.down[1:length(ics$Clicks)] <- NA
for (i in 31:length(ics$Clicks)) {
  if ((ics$Clicks_7[i] < ics$Clicks_30[i]) & (ics$Clicks_7[(i - 1)] > ics$Clicks_30[(i - 1)])) {
    ics$Clicks.cross.down[i] <- 1
  }
  else {
    ics$Clicks.cross.down[i] <- 0
  }
}
sum(ics$Clicks.cross.down, na.rm = T)
Clicks.cross.down <- filter(ics, Clicks.cross.down == 1)

# 7-30 Imps cross up #QAd
ics$Imps.cross.up[1:length(ics$Imps)] <- NA
for (i in 31:length(ics$Imps)) {
  if ((ics$Imps_7[i] > ics$Imps_30[i]) & (ics$Imps_7[(i - 1)] < ics$Imps_30[(i - 1)])) {
    ics$Imps.cross.up[i] <- 1
  }
  else {
    ics$Imps.cross.up[i] <- 0
  }
}
sum(ics$Imps.cross.up, na.rm = T)
Imps.cross.up <- filter(ics, Imps.cross.up == 1)

# 7-30 Imps cross down #QAd
ics$Imps.cross.down[1:length(ics$Imps)] <- NA
for (i in 31:length(ics$Imps)) {
  if ((ics$Imps_7[i] < ics$Imps_30[i]) & (ics$Imps_7[(i - 1)] > ics$Imps_30[(i - 1)])) {
    ics$Imps.cross.down[i] <- 1
  }
  else {
    ics$Imps.cross.down[i] <- 0
  }
}
sum(ics$Imps.cross.down, na.rm = T)
Imps.cross.down <- filter(ics, Imps.cross.down == 1)

crosses <- ics[,c(1,13:18)]
# print.data.frame(crosses)
crosses$Total <- cbind(rowSums(crosses[,-1]))
crosses <- crosses[complete.cases(crosses),]
arrange(ungroup(crosses), desc(Total))
cor(crosses[,2:7])
corrgram::corrgram(crosses[,2:7])


# cross_test <- crosses[,2:7]
# corrplot::corrplot(cor(crosses[,2:7]))
