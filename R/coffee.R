library(Quandl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)

arabica <- Quandl("ODA/PCOFFOTM_USD")
Period  <- rep(NA, nrow(arabica))
arabica  <- cbind(arabica, Period)

glimpse(arabica)
summary(arabica)

bin_80to84 <- list(1980:1984)
bin_85to89 <- list(1985:1989)
bin_90to94 <- list(1990:1994)
bin_95to99 <- list(1995:1999)
bin_00to04 <- list(2000:2004)
bin_05to09 <- list(2005:2009)
bin_10to15 <- list(2010:2015)

arabica$Period  <-  if(year(arabica$Date) in bin_80to84) arabica$Period <- as.factor("80to84")

g1  <- ggplot(arabica, aes(x = Value, alpha = 0.7)) + geom_density(aes(fill = as.factor(arabica$Period)))
g1 + theme_tufte() + ggtitle("Arabica Price Densities, 1980-2015")

g2 <- ggplot(arabica, aes(x = Date, y = Value, size = 1.5)) + geom_line(aes(color = as.factor(arabica$Period))))
g2 + theme_tufte() + ggtitle("Price of Arabica Coffee, IMF")

library(forecast)
library(timeSeries)
arabica$Date  <- as.Date(arabica$Date, format = "%Y/%M/%D")
arabica$Value <- ts(arabica$Value, start = c(1980,1), end = c(2015,5), frequency = 12)

f1 <- stl(arabica$Value,
              s.window = 12, s.degree = 1,
              t.window = 12, t.degree = 1,
              l.window = 12, l.degree = 1,
              robust = T)
plot(f1)
plot(forecast(f1))

acf(arabica,
    lag.max = 36,
    plot = TRUE,
    na.action = na.pass,
    demean = TRUE, main = "DEMO ONLY")

d_adj <- seasadj(f1)
# could I put both the adjusted and unadjusted up on the same graph
plot(arabica$Date, arabica$Value, type = "l", main = "Arabica Prices, NOT Seasonally Adjusted")
plot(d_adj, main = "Arabica Prices, Seasonally Adj.")

f2 <- ets(arabica$Value)
x1 <- forecast(f1, h = 60)
x2 <- forecast(f2, h = 60,
              simulate = T,
              bootstrap = F)
accuracy(x1)
accuracy(x2)
summary(x1)
summary(x2)
plot(x1)
plot(x2)
