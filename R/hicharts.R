library(highcharter)
library(dplyr)
library(tidyr)

births <- read.csv("https://github.com/mine-cetinkaya-rundel/highcharts-in-r/blob/master/data/US_births_2000-2014_SSA.csv")

diff13 <- births %>%
  filter(date_of_month %in% c(6, 13, 20)) %>%
  mutate(day = ifelse(date_of_month == 13, "thirteen", "not_thirteen")) %>%
  group_by(day_of_week, day) %>%
  summarise(mean_births = mean(births)) %>%
  arrange(day_of_week) %>%
  spread(day, mean_births) %>%
  mutate(diff_ppt = ((thirteen - not_thirteen) / not_thirteen) * 100)

hchart(diff13, "scatter", x = day_of_week, y = diff_ppt)

highchart() %>%
  hc_add_series(data = round(diff13$diff_ppt, 4), type = "column",
                name = "Difference, in ppt",
                color = "#b20000", showInLegend = FALSE) %>%
  hc_yAxis(title = list(text = "Difference, in ppt"), allowDecimals = FALSE) %>%
  hc_xAxis(categories = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday", "Sunday"),
           tickmarkPlacement = "on",
           opposite = TRUE) %>%
  hc_title(text = "The Friday the 13th effect",
           style = list(fontWeight = "bold")) %>% 
  hc_subtitle(text = "Difference in the share of U.S. births on 13th of each month 
              from the average of births on the 6th and the 20th,
              1994 - 2004") %>%
  hc_tooltip(valueDecimals = 4,
             pointFormat = "Day: {point.x} <br> Diff: {point.y}") %>%
  hc_credits(enabled = TRUE, 
             text = "Sources: CDC/NCHS, SOCIAL SECURITY ADMINISTRATION",
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_538())
