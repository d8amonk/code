library(dplyr)
library(ggplot2)

VIQ <- read.csv("c:/Users/jvangeete/Desktop/viq_ST_report.csv")
VIQ$DAY <- as.Date(VIQ$DAY, format = '%m/%d/%Y')

VIQ <- filter(VIQ, BRAND == "StraightTalk")
VIQ <- filter(VIQ, FUNNEL == "AWS" | FUNNEL == "MID" | FUNNEL == "DR")

viq <- VIQ %>% 
  group_by(DAY, FUNNEL) %>% 
  mutate(Cost = sum(COST),
         Impressions = sum(IMPRESSIONS),
         Clicks = sum(CLICKS),
         Conversions = sum(CONVERSIONS),
         True_Conversions = sum(TRUE_CONVERSIONS)) %>%
  select(Date = DAY,
         Brand = BRAND,
         Funnel = FUNNEL,
         Cost,
         Impressions,
         Clicks,
         Conversions,
         True_Conversions) %>% ungroup()

distinct(select(viq, Date, Brand, Funnel, Cost, Impressions, Clicks, Conversions, True_Conversions))

g <- ggplot(viq, aes(x = Date, y = Impressions, col = Funnel, size = Cost)) + geom_point() + geom_smooth()
