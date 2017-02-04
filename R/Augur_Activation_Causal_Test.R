##Installing Causal Impact

options(scipen =999)

install.packages("devtools")
library(devtools)
devtools::install_github("google/CausalImpact")
library(CausalImpact)

total_sales <- read.csv("C:/Users/aknorr/Documents/R/Total_sales_041216.csv", header = T)
bf_clicks <-read.csv("C:/Users/aknorr/Documents/R/black_friday dummy.csv", header = T)

total_sales$EVENT_DATE <- as.Date(total_sales$EVENT_DATE, format = "%m/%d/%Y")
bf_clicks$Date <- as.Date(bf_clicks$Date, format = "%m/%d/%Y")

bf_clicks <- bf_clicks[rev(order(bf_clicks$Date)),]

total_sales <- na.omit(total_sales)

summary(total_sales)
t <- 1:832

head(total_sales)
total_sales <- cbind(total_sales, bf_clicks,t)
total_sales$Date <- NULL

zoo_total_sales <- zoo(total_sales$TOTAL_SALES, total_sales$EVENT_DATE)

matplot(total_sales, type = "l")

pre.period <- as.Date(c("2014-01-01", "2015-11-21"))
post.period <- as.Date(c("2015-11-22", "2016-04-11"))

impact <- CausalImpact(zoo_total_sales, pre.period, post.period, model.args = list(nseasons=7, season.duration=1))
plot(impact)
summary(impact, "report")
