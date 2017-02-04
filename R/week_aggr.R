require(dplyr)

x <- read.csv("August Billing.csv")
glimpse(x)


# whitespace trimmer
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

x$COOKIE <- trim(x$COOKIE)

# convert Date as necessary
x$Date <- as.Date(x$SALE_DATE)

# Week counter, %W = weekstart(Monday) %U = weekstart(Sunday)
x$Week <- format(x$Date, format = "%W")

# Think pivot tables with a day -> week aggregator 
out <- x %>% 
  arrange(Date) %>% 
  group_by(Week, COOKIE) %>% 
  mutate(Total_sales = sum(BILLABLE_QUANTITY),
         Total_cost = sum(TOTAL_COST),
         Week_start = min(Date),
         Week_end = max(Date),
         Wk_length = Week_end - Week_start + 1,
         Brand = COOKIE) %>% 
  ungroup() %>% 
  select(Week, Brand, Total_sales, Total_cost, Week_start, Week_end, Wk_length) %>% 
  unique()

# Pay attention to getwd() and setwd()
write.csv(out, "billables_test.csv")
  