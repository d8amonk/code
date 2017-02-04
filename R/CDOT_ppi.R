library(Quandl)
library(ggplot2)
library(ggthemes)
library(dplyr)

setwd("C:/Users/vangeetej/Google Drive/CDOT/Ermias/Jeff")


US_imp_crude  <- Quandl("DOE/MCRIMUS1")
US_exp_crude  <- Quandl("DOE/MCREXUS1")

#same number of (months) rows!
US_oil  <- data.frame(imports = US_imp_crude,
                      exports = US_exp_crude)

head(US_oil)

COL_crude_P  <- Quandl("DOE/F004008__3")
COL_crude_Q  <- Quandl("DOE/MCRFPCO1")

COL_oil_prod  <- data.frame(value = COL_crude_Q$Value,
                            date = COL_crude_Q$Value)
COL_oil_price  <- data.frame(value = COL_crude_P$Value,
                            date = COL_crude_P$Value)
write.csv(US_oil, "US_Crude_ImpExp.csv")
write.csv(COL_crude_P, "COL_crude_P.csv")
write.csv(COL_crude_Q, "COL_crude_Q.csv")

g1 <- ggplot(COL_oil_price, aes(x = date,
                          y = value))
g1 +
  geom_line() +
  geom_line(aes(x = as.Date(COL_oil),
             y = log(COL_oil$fieldprod.Value), col = 'red'))


g2 <- ggplot(US_oil, aes(x = exports.Date))
g2 +
  geom_line(aes(y = log(US_oil$imports.Value), col = 'red')) +
  geom_line(aes(y = log(US_oil$exports.Value))) +
  ggtitle("%chg in Monthly US Oil Imports (Red), Exports (Black)") +
  labs(x = "Date", y = "%") +
  theme_gdocs() +
  theme(legend.position="none")




################################
################################
#########notes on themes########
#########               ########
################################
################################
theme(plot.title = element_text(family = "Trebuchet MS",
                                olor="#666666",
                                face="bold",
                                size=32,
                                hjust=0)) +
theme(axis.title = element_text(family = "Trebuchet MS",
                                color="#666666",
                                face="bold",
                                size=22))

theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())