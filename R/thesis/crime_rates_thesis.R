library(dplyr)
library(ggplot2)
# cops don't show up to stop crime in large numbers
# however, the gear is deployed when cops are in large numbers.
# therefore, unless the type of crime is one where cops show up in large numbers
# then gear doesn't have a strong relationship with crime (save for rape)

# INTERACT OUT_OF_STATE WITH RACE VARS

# rm(list = ls())

#all data
setwd("C:/Users/Jeffrey/Google Drive/School/UCD/Thesis/datasets/Merges/")
full_set <- read.csv("fullset.csv", stringsAsFactors = FALSE)

#just gear
setwd("C:/Users/jvangeete/Google Drive/School/UCD/Thesis/datasets/1033/")
gear <- read.csv("1033prog.csv", stringsAsFactors = FALSE)

#gear sums
# gear_tab <- gear %>% 
#   group_by(Item.Name) %>% 
#   summarise(TotalQ = sum(Quantity),
#          TotalV = sum(Total.Cost)) %>% 
#   distinct(Item.Name, TotalQ, TotalV) %>% 
#   select(Item.Name, TotalQ, TotalV)

# library(xtable)
# gtq <- arrange(gear_tab, desc(TotalQ))
# gtv <- arrange(gear_tab, desc(TotalV))
# xtable(slice(gtv, 1:25), caption = "Gear Sorted by Total Value", caption.placement = "top")

#fix for logs
full_set[full_set == 0] <- 1

# double book
f <- full_set

# make sure you're using the agg-summed vc and pc; +1 for log adj above
all(f$Violent.Crime == f$Murder + f$Rape + f$Robbery + f$Agg.Assault | 
      f$Murder + f$Rape + f$Robbery + f$Agg.Assault + 1, na.rm = TRUE)

f <- f %>% 
  mutate(
  vc_rate = as.numeric(Violent.Crime / (Population/10000)),
  pc_rate = as.numeric(Property.Crime / (Population/10000)),
  murder_rate = as.numeric(Murder / (Population/10000)),
  rape_rate = as.numeric(Rape / (Population/10000)),
  rob_rate = as.numeric(Robbery / (Population/10000)),
  assault_rate = as.numeric(Agg.Assault / (Population/10000)),
  burg_rate = as.numeric(Burglary / (Population/10000)),
  larc_rate = as.numeric(Larceny.Theft / (Population/10000)),
  mvt_rate = as.numeric(Motor.Vehicle.Theft / (Population/10000)),
  arson_rate = as.numeric(Arson / (Population/10000))
  )
# detach(full_set); attach(full_set)

# quantile anlaysis
f <- within(f, vc_q <- as.integer(cut(f$vc_rate, quantile(f$vc_rate, probs=0:4/4, na.rm = T), include.lowest=TRUE)))
f <- within(f, pc_q <- as.integer(cut(f$pc_rate, quantile(f$pc_rate, probs=0:4/4, na.rm = T), include.lowest=TRUE)))
f <- within(f, vc_d <- as.integer(cut(f$vc_rate, quantile(f$vc_rate, probs=0:10/10, na.rm = T), include.lowest=TRUE)))
f <- within(f, pc_d <- as.integer(cut(f$pc_rate, quantile(f$pc_rate, probs=0:10/10, na.rm = T), include.lowest=TRUE)))

# DD ----------------------------------------------------------------------


# # quartiles
pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_q == c(1,2)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_q == c(3,4)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_q == c(1,2)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_q == c(3,4)])

# DD estimate by vc halves
(post.high-post.low) - (pre.high-pre.low) #7,675,742

pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_q == c(1)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_q == c(4)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_q == c(1)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_q == c(4)])

# DD estimate by outer vc quartiles
(post.high-pre.high) - (post.low-pre.low) #1,924,356

pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_q == c(2)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_q == c(3)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_q == c(2)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_q == c(3)])

# DD estimate by inner vc quartiles
(post.high-pre.high) - (post.low-pre.low) #11,782,257

# deciles
pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(1)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(9)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(1)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(9)])

# DD estimate by outer (1st and 9th) vc deciles
(post.high-pre.high) - (post.low-pre.low) #11,127,120

pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(4)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(5)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(4)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(5)])

# DD estimate by inner (4th and 5th) vc deciles
(post.high-pre.high) - (post.low-pre.low) #-4,469,970

pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(3)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(6)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(3)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(6)])

# DD estimate by inner (3rd and 6th) vc deciles
(post.high-pre.high) - (post.low-pre.low) #13,629,647

pre.low <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(2)])
pre.high <- mean(f$Gear_Spend[f$Year == 2006 & f$vc_d == c(7)])
post.low <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(2)])
post.high <- mean(f$Gear_Spend[f$Year == 2013 & f$vc_d == c(7)])

# DD estimate by inner (2nd and 7th) vc deciles
(post.high-pre.high) - (post.low-pre.low) #7,380,184

d1b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 1])
d2b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 2])
d3b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 3])
d4b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 4])
d5b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 5])
d6b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 6])
d7b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 7])
d8b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 8])
d9b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 9])
d10b <- mean(f$Gear_Spend[f$Year == c(2006) & f$vc_d == 10])
db <- data.frame(decile = c(1:10), mean = c(d1b, d2b, d3b, d4b, d5b, d6b, d7b, d8b, d9b, d10b))

d1a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 1])
d2a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 2])
d3a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 3])
d4a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 4])
d5a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 5])
d6a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 6])
d7a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 7])
d8a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 8])
d9a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 9])
d10a <- mean(f$Gear_Spend[f$Year == c(2013) & f$vc_d == 10])
da <- data.frame(decile = c(1:10), mean = c(d1a, d2a, d3a, d4a, d5a, d6a, d7a, d8a, d9a, d10a))

ggplot(db, aes(x=decile, y=mean/1000000)) + geom_smooth(se = F, stat = "smooth") +
  xlab("Violent Crime Decile") + ylab("Mean Military Gear Transfers ($M)") + ggtitle("Distribution of Pre-Period (2006)")
ggplot(da, aes(x=decile, y=mean/1000000)) + geom_smooth(se = F, stat = "smooth") + 
  xlab("Violent Crime Decile") + ylab("Mean Military Gear Transfers ($M)") + ggtitle("Distribution of Post-Period (2013)")

d1b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 1])
d2b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 2])
d3b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 3])
d4b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 4])
d5b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 5])
d6b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 6])
d7b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 7])
d8b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 8])
d9b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 9])
d10b <- mean(f$Gear_Spend[f$Year == c(2006) & f$pc_d == 10])
db <- data.frame(decile = c(1:10), mean = c(d1b, d2b, d3b, d4b, d5b, d6b, d7b, d8b, d9b, d10b))

d1a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 1])
d2a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 2])
d3a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 3])
d4a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 4])
d5a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 5])
d6a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 6])
d7a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 7])
d8a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 8])
d9a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 9])
d10a <- mean(f$Gear_Spend[f$Year == c(2013) & f$pc_d == 10])
da <- data.frame(decile = c(1:10), mean = c(d1a, d2a, d3a, d4a, d5a, d6a, d7a, d8a, d9a, d10a))

ggplot(db, aes(x=decile, y=mean)) + geom_smooth(se = F, stat = "smooth")
ggplot(da, aes(x=decile, y=mean)) + geom_smooth(se = F, stat = "smooth")

# ----------------------------------------------------------------------

# f_q <- !is.na(select(f, contains("rate")))
# library(corrplot)
# cor(f$vc_rate, f$pc_rate, use = "complete.obs")
# cor(f$vc_q, f$pc_q, use = "complete.obs")

library(plm)
library(gplots)

pairs(f[4:15])
pairs(f[16:49])
pairs(f[16:49], log = 'xy')

plotmeans(f$vc_rate ~ f$Year)
plotmeans(f$Gear_Spend/1000000 ~ f$Year,
          xlab="Year",
          ylab="Gear Transfers ($M)",
          main="Means and CI95 of Gear Transfers, by Year")

#all models are fe; first are levels, then logs
#base model, levels
tab3 <- lm(log(Gear_Spend) ~ Violent.Crime + Property.Crime + log(Population), f)
tab33 <- lm(log(Gear_Spend) ~ vc_rate + pc_rate + log(Population), f)
summary(tab3) 
summary(tab33)
stargazer(tab3, tab33, title="Log(Gear Transfers) on Aggregate Measures of Crime Levels and Rates")

#add granular crime measures
bm.2 <- lm(log(Gear_Spend) ~ Murder + 
              Rape +
              Robbery +
              Agg.Assault +
              Burglary +
              Larceny.Theft +
              Motor.Vehicle.Theft +
              Arson +
              log(Population), f)
# summary(bm.2)
stargazer(bm.2, title="Granular Measures of Crime Levels")

bm.22 <- lm(log(Gear_Spend) ~ murder_rate + 
              rape_rate +
              rob_rate +
              assault_rate +
              burg_rate +
              larc_rate +
              mvt_rate +
              arson_rate +
              log(Population), f)
# summary(bm.2)
stargazer(bm.22, title="Granular Measures of Crime Rates")


#add linear year
bm.3 <- lm(log(Gear_Spend) ~ murder_rate + 
             rape_rate +
             rob_rate +
             assault_rate +
             burg_rate +
             larc_rate +
             mvt_rate +
             arson_rate +
             log(Population) +
             as.factor(Year), f)
# summary(bm.3)
stargazer(bm.3, title="Aggregate Crime Measures, Linear Year Trend")


#add specificity to crime obs
bm.4 <- plm(log(Gear_Spend) ~ murder_rate + 
              rape_rate +
              rob_rate +
              assault_rate +
              burg_rate +
              larc_rate +
              mvt_rate +
              arson_rate +
              Population, f[complete.cases(f),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(bm.4)
bm.44 <- plm(log(Gear_Spend) ~ murder_rate + 
              rape_rate +
              rob_rate +
              assault_rate +
              burg_rate +
              larc_rate +
              mvt_rate +
              arson_rate +
              Hispanic +
              Black +
              NativeIndian +
              Asian + 
              Islander +
              OtherRace + 
              log(Population), f[complete.cases(f),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(bm.44)
stargazer(bm.44)
#need to CSE
library(lmtest)
# bm.4 <- coeftest(bm.4, vcov=vcovHC)
bm.44 <- coeftest(bm.44, vcov=vcovHC)
# bm.444 <- coeftest(bm.444, vcov=vcovHC)
stargazer(bm.44, title="Aggregate Crime Rates; Race, 2-Way FEs, CSEs")

bm.5 <- plm(log(Gear_Spend) ~ lag(murder_rate, 1) + 
              lag(rape_rate, 1) +
              lag(rob_rate, 1) +
              lag(assault_rate, 1) +
              lag(burg_rate, 1) +
              lag(larc_rate, 1) +
              lag(mvt_rate, 1) +
              lag(arson_rate, 1) +
              lag(Hispanic, 1) +
              lag(Black, 1) +
              lag(NativeIndian, 1) +
              lag(Asian, 1) + 
              lag(Islander, 1) +
              lag(OtherRace, 1) + 
              lag(Population, 1), f[complete.cases(f),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(bm.5)

bm.5 <- plm(diff(Gear_Spend, differences = 1) ~ lag(murder_rate, 1) + 
              lag(rape_rate, 1) +
              lag(rob_rate, 1) +
              lag(assault_rate, 1) +
              lag(burg_rate, 1) +
              lag(larc_rate, 1) +
              lag(mvt_rate, 1) +
              lag(arson_rate, 1) +
              lag(Hispanic, 1) +
              lag(Black, 1) +
              lag(NativeIndian, 1) +
              lag(Asian, 1) + 
              lag(Islander, 1) +
              lag(OtherRace, 1) + 
              lag(Population, 1), f[complete.cases(f),], index=c("State", "Year"), model= "within", effect = "twoway")
summary(bm.5)


# # education?
# bm.5 <- plm(Gear_Spend ~ black_educ + 
#               Rape +
#               Robbery + 
#               Agg.Assault + 
#               Burglary + 
#               Larceny.Theft + 
#               Motor.Vehicle.Theft + 
#               black_tot + 
#               Population +
#               Year, full2, index=c("State", "Year"), model= "within", effect = "twoway")
# summary(bm.5)

#logged models
#fe log model w agg'd crime vars (no results)
ln.1 <- plm(log(Gear_Spend) ~ log(Violent.Crime) + 
               log(Property.Crime), f, index=c("State", "Year"), model= "within", effect = "twoway")
summary(ln.1) 

#add pop control (no results)
ln.2 <- plm(log(Gear_Spend) ~ log(Violent.Crime) + 
               log(Property.Crime) + 
               log(Population), full2, index=c("State", "Year"), model= "within", effect = "twoway")
summary(ln.2) 

#add specifity to crime obs (no results)
ln.3 <- plm(log(Gear_Spend) ~ log(Murder) + 
               log(Rape)+ 
               log(Robbery) + 
               log(Agg.Assault) + 
               log(Burglary) + 
               log(Larceny.Theft) + 
               log(Motor.Vehicle.Theft) + 
               log(Population), f, index=c("State", "Year"), model= "within", effect = "twoway")
summary(ln.3)

#linear year (no results)
ln.4 <- plm(log(Gear_Spend) ~ log(Murder) + 
               log(Rape)+ 
               log(Robbery) + 
               log(Agg.Assault) + 
               log(Burglary) + 
               log(Larceny.Theft) + 
               log(Motor.Vehicle.Theft) + 
               log(Population) +
               Year, f, index=c("State", "Year"), model= "within", effect = "twoway")
summary(ln.4)


l1 <- lm(Property.Crime ~ lag(log(Gear_Spend), 2), data = f[f$Property.Crime < 167000,])
l1 <- lm(Property.Crime ~ lag(log(Gear_Spend), 1) + lag(log(Gear_Spend), 2), lag(log(Gear_Spend), 3))

summary(l1)




library(multiwayvcov)
library(lmtest)

#run a breusch-pagan, null = HomSk resid
bptest(bm.1) 
bptest(bm.2) 
bptest(bm.3) 
bptest(bm.4) 

bptest(ln.1) 
bptest(ln.2) 
bptest(ln.3) 
bptest(ln.4) 

library(stargazer)
summary(fixed)
(bc.1 <- coeftest(bm.1, vcovHC)) #option vcovHC clusters your SEs for you
(bc.2 <- coeftest(bm.2, vcovHC)) #option vcovHC clusters your SEs for you
(bc.3 <- coeftest(bm.3, vcovHC)) #option vcovHC clusters your SEs for you
(bc.4 <- coeftest(bm.4, vcovHC)) #option vcovHC clusters your SEs for you
(lc.1 <- coeftest(ln.1, vcovHC)) #option vcovHC clusters your SEs for you
(lc.2 <- coeftest(ln.2, vcovHC)) #option vcovHC clusters your SEs for you
(lc.3 <- coeftest(ln.3, vcovHC)) #option vcovHC clusters your SEs for you
(lc.4 <- coeftest(ln.4, vcovHC)) #option vcovHC clusters your SEs for you

# forward stepwise to find the culprits of singularity
library(MASS)
step.bm.1 <- stepAIC(bm.1, direction="both")
step.bm.2 <- stepAIC(bm.2, direction="both")
step.bm.3 <- stepAIC(bm.3, direction="both")
step.bm.4 <- stepAIC(bm.4, direction="both")

step.ln.1 <- stepAIC(ln.1, direction="both")
step.ln.2 <- stepAIC(ln.2, direction="both")
step.ln.3 <- stepAIC(ln.3, direction="both")
step.ln.4 <- stepAIC(ln.4, direction="both")

step.bm.1$anova # display results
step.bm.2$anova # display results
step.bm.3$anova # display results
step.bm.4$anova # display results

step.ln.1$anova # display results
step.ln.2$anova # display results
step.ln.3$anova # display results
step.ln.4$anova # display results

library(stargazer)
stargazer(bm.1, bm.2, bm.3, bm.4, digits = 2)
stargazer(ln.1, ln.2, ln.3, ln.4, digits = 2)

# or do it manually # NERRRP

# manual calc of SEs ------------------------------------------------------


# Cluster by state
vcov_state <- cluster.vcov(fixed, full2$State)
coeftest(fixed, vcov_state)

# Cluster by year
vcov_year <- cluster.vcov(fixed, full2$Year)
coeftest(fixed, vcov_year)

# Double cluster by firm and year
vcov_both <- cluster.vcov(m1, cbind(petersen$firmid, petersen$year))
coeftest(m1, vcov_both)

# Replicate Mahmood Arai's double cluster by firm and year
vcov_both <- cluster.vcov(m1, cbind(petersen$firmid, petersen$year), use_white = FALSE)
coeftest(m1, vcov_both)
# ------------------------------------------------------------------------
  

