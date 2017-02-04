library(gdata)
library(dplyr)
library(ggplot2)

vend_piv <- read.csv("z:/DA&DS/Data Scientists Only/Jeff/Coverage Analysis/vend.csv")
vend <- read.csv("z:/DA&DS/Data Scientists Only/Jeff/Coverage Analysis/vendor_imps_uimps_byday.csv")

View(vend)
vend$EVENT_DATE <- as.Date(vend$EVENT_DATE, format = '%m/%d/%Y')



ggplot(vend, aes(x = UNIQUE_VENDOR_REACH, color = VENDOR, fill = VENDOR, alpha = 0.1)) + 
  geom_density(aes(y = ..density..)) + facet_grid
  xlim(c(0,70))
