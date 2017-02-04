require(dplyr)
require(ggplot2)
require(ggthemes)
require(RColorBrewer)
require(tidyr)

# will need to functionalize these, for now hardcode values
b_daily <- 1561
incr_daily <- 6

start <- as.Date('2016-06-30')
begin_30 <- as.Date('2016-07-05')
end <- as.Date('2016-12-31')

begin_30xts <- gsub('-', '', as.character(begin_30))
end_30 <- begin_30 + 30
end_30xts <- gsub('-', '', as.character(end_30))
today <- Sys.Date()
range_30xts <- paste(begin_30xts, '/', end_30xts, sep = '')


dt_len <- as.integer(as.Date(end) - start)

# go get actual sales data
source("Z:/DA&DS/Data Scientists Only/Reports/daily_send_email_v1_4.R")

# run DSGM function
email_stats()

# build obj for scenario data
x <- matrix(rep(NA, 10*dt_len), ncol = 10)
colnames(x) <- c("baseline", "cumul_baseline", 
                 "scenario_1", "cumul_scenario_1", 
                 "scenario_2", "cumul_scenario_2", 
                 "scenario_3", "cumul_scenario_3",
                 "actual", "cumul_actual")

dt_ts <- data.frame(Date = seq.Date(from = start+1, to = end, by = "days"), x)
dt_ts_30 <- filter(dt_ts, Date >= begin_30 & Date <= end_30)
dt_ts_30$baseline <- b_daily
dt_ts_30$cumul_baseline <- cumsum(dt_ts_30$baseline)
dt_ts_30$scenario_1 <- dt_ts_30$baseline + 1:31*6
dt_ts_30$cumul_scenario_1 <- cumsum(dt_ts_30$scenario_1)
dt_ts_30$scenario_2 <- dt_ts_30$baseline + 1:31*9
dt_ts_30$cumul_scenario_2 <- cumsum(dt_ts_30$scenario_2)
dt_ts_30$scenario_3 <- dt_ts_30$baseline + 1:31*13
dt_ts_30$cumul_scenario_3 <- cumsum(dt_ts_30$scenario_3)

# # fuggit
# dt_ts <- xts(x, start + 1:as.integer(dt_len))
# 
# dt_ts_30 <- dt_ts[range_30xts]
# dt_ts_30$Date <- as.Date.numeric(index(dt_ts_30))

ics_ts_30 <- filter(ics, Date >= begin_30 & Date <= end_30)

dtics_30 <- left_join(dt_ts_30, ics_ts_30, on = "Date")

dtics_30$actual <- dtics_30$Sales
dtics_30$Sales <- NULL
dtics_30$cumul_actual <- cumsum(dtics_30$actual)

head(dtics_30)
tail(dtics_30)

dtics_30$diff_b1 <- dtics_30$cumul_scenario_1 - dtics_30$cumul_baseline 
dtics_30$diff_b2 <- dtics_30$cumul_scenario_2 - dtics_30$cumul_baseline 
dtics_30$diff_b3 <- dtics_30$cumul_scenario_3 - dtics_30$cumul_baseline 
dtics_30$diff_ba <- dtics_30$cumul_actual - dtics_30$cumul_baseline 

dtics_tidy <- dtics_30

# gather - dat %>% gather(variable, date, -a, -b)
dtics_tidy <- dtics_tidy %>% gather(which_diff, amount, 36:39)

# cumul_actual against cumul_baseline
ggplot(dtics_tidy, aes(Date, amount, col = which_diff, fill = which_diff)) + 
  geom_line(lwd = 1.1) +
  ylab("Cumulative Incremental Sales") +
  xlab("") +
  ggtitle("DA&DS Spend Recommendation Tracker") +
  theme_bw() +
  theme(legend.position="right") +
  scale_color_manual(values=c("gold", "orange", "green", "black"),
                    name="Sales Scenario\n(Baseline = 1,561 Daily Sales Avg.)",
                    labels=c("\nScenario 1 = Baseline + 6\n", 
                             "\nScenario 2 = Baseline + 9\n", 
                             "\nScenario 3 = Baseline + 13\n",
                             "\nActual\n"))



