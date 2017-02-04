library(nzr)
library(dplyr)

# connect to nz
nzConnect("admin", "password", "192.168.100.5", "CONVERSION")

# get the ds tables
ds <- nz.data.frame("DAILY_SEND")
ds.data <- as.data.frame(ds)
ds.data$SALE_DATE <- substr(ds.data$SALE_DATE, 1,11)
ds.data$SALE_DATE <- as.Date(ds.data$SALE_DATE, format = "%Y-%m-%d")

# get the pub data
nzConnect("admin", "password", "192.168.100.5", "CLIENT_TRACFONE", force = TRUE)
client_tf <- nz.data.frame("DAILY_SEND_PUB_DATA")
client_tf.data <- as.data.frame(client_tf)
client_tf.data$EVENT_DATE <- substr(client_tf.data$EVENT_DATE, 1,11)
client_tf.data$EVENT_DATE <- as.Date(client_tf.data$EVENT_DATE, format = "%Y-%m-%d")

# pivot device sales by day, brand
ds <- ds.data %>% 
  filter(DEVICE_OR_SIM == "DEVICE") %>% 
  group_by(SALE_DATE, SKU_BRAND) %>%
  mutate(Dev_Sales = sum(BILLABLE_QUANTITY)) %>% 
  arrange(SALE_DATE)

# drop the billable quants that were used to sum total (device) sales
ds$BILLABLE_QUANTITY <- NULL
# select distinct rows (still 4x per day for brands)
ds <- distinct(ds)

#######################################
#######################################
########### plotting ##################
#######################################
#######################################
library(ggplot2)
library(ggthemes)
g <- ggplot(ds.data, aes(SALE_DATE, TOTAL_IMP, color = BRAND, size = TOTAL_COST/TOTAL_SALES))
g + geom_point() + geom_smooth() + theme_calc() + scale_color_tableau()
