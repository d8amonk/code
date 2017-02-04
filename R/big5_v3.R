# housekeeping
library(readxl)
library(dplyr)
library(quantmod)
options(scipen = 999)

# mint for pillow
wow <- read_excel('c:/Users/jvangeete/Desktop/Asks/Copy of big5_model_Tony_rightbackatcha.xlsx', sheet = 1)
raw <- read.csv('c:/Users/jvangeete/Desktop/Asks/big5_model_Tony_newRAW.csv')

# week on week calcs
wow$Non_branded <- as.numeric(wow$Non_branded)
wow$Conquesting <- as.numeric(wow$Conquesting)
wow$RTG <- as.numeric(wow$RTG)
wow <- arrange(wow, Week_Start)
wow$week <- 1:nrow(wow)

# daily calcs
raw$date <- as.Date(raw$date, format = '%m/%d/%Y')
raw <- arrange(raw, date)
raw$week <-  as.numeric(strftime(raw$date, format = "%W")) + 1
raw <- raw %>% 
  group_by(week, campaign) %>% 
  mutate(week_imps = sum(imps)) %>% 
  ungroup() %>% 
  filter(channel != 'OTHER')

# make weights
rawow <- left_join(wow, raw, by = 'week')
rawow$day_wt <- rawow$imps/rawow$week_imps 

# do redemption calcs with conditional subsets (ifloopize?) 
rawow$day_redemp <- NA
brand_logi <- rawow$campaign == 'BRANDED_SEARCH'
rawow$day_redemp[brand_logi] <- rawow$day_wt[brand_logi] * rawow$Branded[brand_logi]
nonbrand_logi <- rawow$campaign == 'NON_BRANDED_SEARCH'
rawow$day_redemp[nonbrand_logi] <- rawow$day_wt[nonbrand_logi] * rawow$Non_branded[nonbrand_logi]
cq_logi <- rawow$campaign == 'CONQUESTING_SEARCH'
rawow$day_redemp[cq_logi] <- rawow$day_wt[cq_logi] * rawow$Conquesting[cq_logi]
cqd_logi <- rawow$campaign == 'SPOTZOT_CQ_DISPLAY'
rawow$day_redemp[cqd_logi] <- rawow$day_wt[cqd_logi] * rawow$Spotzot_CQ[cqd_logi]
trad_logi <- rawow$campaign == 'SPOTZOT_TRADITIONAL_DISPLAY'
rawow$day_redemp[trad_logi] <- rawow$day_wt[trad_logi] * rawow$Spotzot_Trad[trad_logi]

rawow$day_redemp <- round(rawow$day_redemp,0)

# by date, campaign sums # QA'd on 4.13
camp_grp <- rawow %>%
  group_by(date, campaign) %>% 
  mutate(coupon_tot = sum(coupon),
         clicks_tot = sum(clicks),
         cost_tot = sum(cost),
         imps_tot = sum(imps),
         est_redemptions = sum(day_redemp)) %>%
           select(date,
                  campaign,
                  coupon = coupon_tot,
                  clicks = clicks_tot,
                  cost = cost_tot,
                  imps = imps_tot,
                  est_redemptions) %>%
           distinct() %>%
           arrange()

plot(density(camp_grp$imps, na.rm = T))

# subset dataset into camps
branded <- subset(camp_grp, campaign == 'BRANDED_SEARCH')
conquest <- subset(camp_grp, campaign == 'CONQUESTING_SEARCH')
gdn <- subset(camp_grp, campaign == 'GDN_RT_DISPLAY')
nonbranded <- subset(camp_grp, campaign == 'NON_BRANDED_SEARCH')
cq_display <- subset(camp_grp, campaign == 'SPOTZOT_CQ_DISPLAY')
trad_display <- subset(camp_grp, campaign == 'SPOTZOT_TRADITIONAL_DISPLAY')

# modeling redemptions
branded_imps_lm <- lm(est_redemptions ~ imps, branded)
conquest_imps_lm <- lm(est_redemptions ~ imps, conquest)
# gdn_imps_lm <- lm(est_redemptions ~ imps, gdn)
nonbranded_imps_lm <- lm(est_redemptions ~ imps, nonbranded)
cq_display_imps_lm <- lm(est_redemptions ~ imps, cq_display)
trad_display_imps_lm <- lm(est_redemptions ~ imps, trad_display)

branded_clicks_lm <- lm(est_redemptions ~ clicks, branded)
conquest_clicks_lm <- lm(est_redemptions ~ clicks, conquest)
# gdn_clicks_lm <- lm(est_redemptions ~ clicks, gdn)
nonbranded_clicks_lm <- lm(est_redemptions ~ clicks, nonbranded)
cq_display_clicks_lm <- lm(est_redemptions ~ clicks, cq_display)
trad_display_clicks_lm <- lm(est_redemptions ~ clicks, trad_display)

branded_coupon_lm <- lm(est_redemptions ~ coupon, branded)
conquest_coupon_lm <- lm(est_redemptions ~ coupon, conquest)
# gdn_coupon_lm <- lm(est_redemptions ~ coupon, gdn)
nonbranded_coupon_lm <- lm(est_redemptions ~ coupon, nonbranded)
# cq_display_coupon_lm <- lm(est_redemptions ~ coupon, cq_display)
# trad_display_coupon_lm <- lm(est_redemptions ~ coupon, trad_display)

branded_coef <- branded_imps_lm$coefficients
branded_rsqr <- summary(branded_imps_lm)$adj.r.squared 
conq_coef <- cq_display_imps_lm$coefficients
conq_rsqr <- summary(cq_display_imps_lm)$adj.r.squared

model_results <- cbind(branded_coef, branded_rsqr)
