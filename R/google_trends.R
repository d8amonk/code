library(gtrendsR)
library(dplyr)

usr <- "jeffrey.vangeete@gmail.com"
psw <- "DILLYgaf!!22"
gconnect(usr, psw) 
term_trend <- gtrends(c("cheap phones", "debt"), res ="NA")

glimpse(term_trend)

plot(term_trend)
