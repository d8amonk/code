# load libraries
require(ggplot2)
require(ggthemes)
require(dplyr)
require(corrgram)
require(TTR)
require(stargazer)
require(lubridate)
require(scales)
require(gridExtra)

# data
# spend <- read.csv("c:/Users/jvangeete/Desktop/spend_est_data.csv")
uimps <- read.csv("c:/Users/jvangeete/Desktop/uimps.csv")
nonimps <- read.csv("c:/Users/jvangeete/Desktop/nonimps.csv")
coverage <- read.csv("c:/Users/jvangeete/Desktop/coverage_perc_data.csv")
transacts <- read.csv("c:/Users/jvangeete/Desktop/transacts.csv")

# date transforms
# spend$Date <- as.Date(spend$Date, format = '%m/%d/%Y')
uimps$Date <- as.Date(uimps$Date, format = '%m/%d/%Y')
nonimps$Date <- as.Date(nonimps$Date, format = '%m/%d/%Y')
coverage$Date <- as.Date(coverage$DATE, format = '%m/%d/%Y' )
transacts$Date <- as.Date(transacts$DATE, format = '%m/%d/%Y' )

# Dates g2g?
all.equal(class(uimps$Date), class(nonimps$Date), class(coverage$Date), class(transacts$Date)) # class(spend$Date)

# Calculuate the rolling 30-day sums with runSum from TTR BEFORE you join!
nonimps <- arrange(nonimps, Date)
nonimps$rs_nonimps <- runSum(nonimps$Nonimps, n = 30, cumulative = FALSE)
nonimps$rs_SEMimps <- runSum(nonimps$SEM_imps, n = 30, cumulative = FALSE)
nonimps$rs_CPAimps <- runSum(nonimps$CPA_imps, n = 30, cumulative = FALSE)
nonimps$rs_MFdispimps <- runSum(nonimps$MF_disp_imps, n = 30, cumulative = FALSE)
nonimps$rs_MFSEMimps <- runSum(nonimps$MF_SEM_imps, n = 30, cumulative = FALSE)

# join it all together
# spend_uimps <- left_join(spend, uimps, by = "Date")
spend_uimps_nonimps <- left_join(uimps, nonimps, by = "Date")
spend_uimps_nonimps_coverage <- left_join(spend_uimps_nonimps, coverage, by = "Date")
fullset <- left_join(spend_uimps_nonimps_coverage, transacts, by = "Date")
rm(spend_uimps, spend_uimps_nonimps, spend_uimps_nonimps_coverage)

write.csv(fullset, "c:/Users/jvangeete/Desktop/fullset.csv")

# start from here if you've already joined
# fullset <- read.csv("c:/Users/jvangeete/Desktop/fullset.csv")
fullset$Date <- as.Date(fullset$Date, format = '%m/%d/%Y')

# construct various day dummies
# day of week
fullset$DOW <- as.factor(weekdays(fullset$Date))
print(levels(fullset$DOW))
fullset$DOW <- factor(fullset$DOW, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday"))
table(fullset$DOW)

# day of month
fullset$DOM <- day(fullset$Date)
table(fullset$DOM)

# payday? (note 1,2,3-day variants can be used)
fullset$Payday3 <- as.factor(ifelse(fullset$DOM == 1 |
                                       fullset$DOM == 2 | 
                                       fullset$DOM == 3 | 
                                       fullset$DOM == 15 | 
                                       fullset$DOM == 16 | 
                                       fullset$DOM == 17, 1, 0))
table(fullset$Payday3)

# weekend-day?
fullset$Weekend <- as.factor(ifelse(fullset$DOW == 'Saturday' | fullset$DOW == 'Sunday', 1, 0 ))
table(fullset$Weekend)

# month
fullset$month <- as.factor(months(fullset$Date))

# 1 - not really sure why to do this, but it keeps the ratio < 1;
# >1 is hard to interpret in the tables
fullset$p_nonimps <- fullset$rs_nonimps / (fullset$rs_nonimps + fullset$Uimps) 
fullset$p_SEMimps <- fullset$rs_SEMimps / (fullset$rs_SEMimps + fullset$Uimps)
fullset$p_CPAimps <- fullset$rs_CPAimps / (fullset$rs_CPAimps + fullset$Uimps)
fullset$p_MFdispimps <- fullset$rs_MFdispimps / (fullset$rs_MFdispimps + fullset$Uimps)
fullset$p_MFSEMimps <- fullset$rs_MFSEMimps / (fullset$rs_MFSEMimps + fullset$Uimps)

# 2??
# fullset$p_nonimps <- fullset$Uimps / fullset$rs_nonimps 
# fullset$p_SEMimps <- fullset$Uimps / fullset$rs_SEMimps
# fullset$p_CPAimps <- fullset$Uimps / fullset$rs_CPAimps
# fullset$p_MFdispimps <- fullset$Uimps / fullset$rs_MFdispimps
# fullset$p_MFSEMimps <- fullset$Uimps / fullset$rs_MFSEMimps

# frequency
fullset$f_nonimps <- fullset$rs_nonimps / fullset$Uimps 
fullset$f_SEMimps <- fullset$rs_SEMimps / fullset$Uimps
fullset$f_CPAimps <- fullset$rs_CPAimps / fullset$Uimps
fullset$f_MFdispimps <- fullset$rs_MFdispimps / fullset$Uimps
fullset$f_MFSEMimps <- fullset$rs_MFSEMimps / fullset$Uimps

# build trimmed date set and brand sets
fullset_backup <- fullset
fullset <- fullset[complete.cases(fullset),]

trimmed_st <- filter(fullset, Date >= '2016-02-08' & Brand == 'ST')
trimmed_tw <- filter(fullset, Date >= '2016-02-08' & Brand == 'TW')
trimmed_tf <- filter(fullset, Date >= '2016-02-08' & Brand == 'TF')
trimmed_nt <- filter(fullset, Date >= '2016-02-08' & Brand == 'NT')

# lm models for non-u imps over u.imps
summary(rs_lm1 <- lm(Uimps ~ Nonimps, data = fullset)) 
summary(rs_lm2 <- lm(Uimps ~ SEM_imps, data = fullset)) 
summary(rs_lm3 <- lm(Uimps ~ CPA_imps, data = fullset)) 
summary(rs_lm4 <- lm(Uimps ~ MF_disp_imps, data = fullset)) 
summary(rs_lm5 <- lm(Uimps ~ MF_SEM_imps, data = fullset)) 

stargazer(rs_lm1, rs_lm2, rs_lm3, rs_lm4, rs_lm5, digits = 2)

# lm models for proportion of non-u.imps 
summary(p_lm1 <- lm(Uimps ~ p_nonimps, data = fullset)) 
summary(p_lm2 <- lm(Uimps ~ p_SEMimps, data = fullset)) 
summary(p_lm3 <- lm(Uimps ~ p_CPAimps, data = fullset)) 
summary(p_lm4 <- lm(Uimps ~ p_MFdispimps, data = fullset)) 
summary(p_lm5 <- lm(Uimps ~ p_MFSEMimps, data = fullset)) 

stargazer(p_lm1, p_lm2, p_lm3, p_lm4, p_lm5, digits = 2)

# lm models for coverage outcome of above models
summary(p_lm1 <- lm(COV_PERC ~ p_nonimps, data = fullset)) 
summary(p_lm2 <- lm(COV_PERC ~ p_SEMimps, data = fullset)) 
summary(p_lm3 <- lm(COV_PERC ~ p_CPAimps, data = fullset)) 
summary(p_lm4 <- lm(COV_PERC ~ p_MFdispimps, data = fullset)) 
summary(p_lm5 <- lm(COV_PERC ~ p_MFSEMimps, data = fullset)) 

stargazer(p_lm1, p_lm2, p_lm3, p_lm4, p_lm5, digits = 2)

# ffffffffffffffffffffffffffffffffffffffffffffffffff
# lm models for f of non-u.imps 
summary(f_lm1 <- lm(Uimps ~ f_nonimps, data = fullset))
mean(fullset$f_nonimps)
summary(f_lm2 <- lm(Uimps ~ f_SEMimps, data = fullset))
mean(fullset$f_SEMimps)
summary(f_lm3 <- lm(Uimps ~ f_CPAimps, data = fullset)) 
mean(fullset$f_CPAimps)
summary(f_lm4 <- lm(Uimps ~ f_MFdispimps, data = fullset)) 
mean(fullset$f_MFdispimps)
summary(f_lm5 <- lm(Uimps ~ f_MFSEMimps, data = fullset)) 
mean(fullset$f_MFSEMimps)
summary(f_lm6 <- lm(Uimps ~ f_SEMimps + f_CPAimps + f_MFdispimps + f_MFSEMimps, data = fullset)) 

stargazer(f_lm1, f_lm2, f_lm3, f_lm4, f_lm5, f_lm6, digits = 2, no.space = T, align = T)

summary(lm1 <- lm(Uimps ~ rs_nonimps, data = fullset))
mean(fullset$Nonimps)
summary(lm2 <- lm(Uimps ~ rs_SEMimps, data = fullset))
mean(fullset$SEM_imps)
summary(lm3 <- lm(Uimps ~ rs_CPAimps, data = fullset)) 
mean(fullset$CPA_imps)
summary(lm4 <- lm(Uimps ~ rs_MFdispimps, data = fullset)) 
mean(fullset$MF_disp_imps)
summary(lm5 <- lm(Uimps ~ rs_MFSEMimps, data = fullset)) 
mean(fullset$MF_SEM_imps)
summary(lm6 <- lm(Uimps ~ rs_SEMimps + rs_CPAimps + rs_MFdispimps + rs_MFSEMimps, data = fullset)) 

stargazer()

# gut check(don't use nonimps = sum(other imps))
  lm2$coefficients[2]*mean(fullset$SEM_imps) +
  lm3$coefficients[2]*mean(fullset$CPA_imps) +
  lm4$coefficients[2]*mean(fullset$MF_disp_imps) +
  lm5$coefficients[2]*mean(fullset$MF_SEM_imps)
  # should equal approx mean(uimps)
  


stargazer(lm1, lm2, lm3, lm4, lm5, lm6, digits = 2)
stargazer(lm6, digits = 2)

# ffffffffffffffffffffffffffffffffffffffffffffffffff
# lm models for f coverage of above models
summary(f_lm1 <- lm(COV_PERC ~ f_nonimps, data = fullset)) 
summary(f_lm2 <- lm(COV_PERC ~ f_SEMimps, data = fullset)) 
summary(f_lm3 <- lm(COV_PERC ~ f_CPAimps, data = fullset)) 
summary(f_lm4 <- lm(COV_PERC ~ f_MFdispimps, data = fullset)) 
summary(f_lm5 <- lm(COV_PERC ~ f_MFSEMimps, data = fullset)) 

stargazer(f_lm1, f_lm2, f_lm3, f_lm4, f_lm5, digits = 2)

# all brands rs_ and p_ non-unique effect on unique
summary(lm_int1 <- lm(Uimps ~ rs_SEMimps + rs_CPAimps + rs_MFdispimps + rs_MFSEMimps + p_SEMimps + p_CPAimps + p_MFdispimps + p_MFSEMimps, fullset, na.action = na.omit ))
summary(lm_int2 <- lm(Uimps ~ rs_SEMimps*p_SEMimps + rs_CPAimps*p_CPAimps + rs_MFdispimps*p_MFdispimps + rs_MFSEMimps*p_MFSEMimps, data = fullset, na.action = na.omit ))

stargazer(lm_int1, lm_int2, digits = 2)

# ST only rs_ and p_ non-unique effect on unique
st_filter <- filter(fullset, Brand == 'ST')
summary(lm_int3 <- lm(Uimps ~ rs_SEMimps + rs_CPAimps + rs_MFdispimps + rs_MFSEMimps + p_SEMimps + p_CPAimps + p_MFdispimps + p_MFSEMimps, data = st_filter, na.action = na.omit ))
summary(lm_int4 <- lm(Uimps ~ rs_SEMimps*p_SEMimps + rs_CPAimps*p_CPAimps + rs_MFdispimps*p_MFdispimps + rs_MFSEMimps*p_MFSEMimps, data = st_filter, na.action = na.omit ))

stargazer(lm_int3, lm_int4, digits = 2)

# trim the dates # REMEMBER that it may be better to subset with scale_x_axis limits on Date!
myfunc <- function(start, end){fullset[fullset$Date >= start & fullset$Date <= end,]}

date1 <- as.Date("2016-02-08")
date2 <- as.Date("2016-05-01")

fullset <- myfunc(date1, date2)
rm(date1, date2)
fullset$TRANSACTIONS <- NULL

# Viz
linear_model <- lm(COV_PERC ~ Uimps, data = fullset)

ggplot(fullset, aes(x = Date, y = rs_nonimps, color = Brand)) + geom_line()

ggplot(fullset, aes(Uimps, COV_PERC, color = Brand)) + 
  geom_point() + 
  geom_abline(intercept = coef(linear_model)[1],
              slope = coef(linear_model)[2])

ggplot(fullset, aes(Uimps, COV_PERC, color = Brand)) + 
  geom_point() + 
  geom_abline(intercept = coef(linear_model)[1],
              slope = coef(linear_model)[2]) +
  facet_grid(Brand ~ ., scales = "free")

ggplot(fullset, aes(Date, p_nonimps,color = Brand)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 month",
               labels= date_format("%b-%Y"),
               limits = as.Date(c('2016-02-08','2016-05-08')))

ggplot(fullset, aes(Date, p_SEMimps,color = Brand)) + geom_line() +
  geom_line() + 
  scale_x_date(date_breaks = "1 month",
               labels= date_format("%b-%Y"),
               limits = as.Date(c('2016-02-08','2016-05-08')))

ggplot(fullset, aes(Date, p_CPAimps,color = Brand)) + geom_line() + 
  geom_line() + 
  scale_x_date(date_breaks = "1 month",
               labels= date_format("%b-%Y"),
               limits = as.Date(c('2016-02-08','2016-05-08')))

ggplot(fullset, aes(Date, p_MFdispimps,color = Brand)) + geom_line() +
  geom_line() + 
  scale_x_date(date_breaks = "1 month",
               labels= date_format("%b-%Y"),
               limits = as.Date(c('2016-02-08','2016-05-08')))

ggplot(fullset, aes(Date, p_MFSEMimps,color = Brand)) + geom_line() +
 geom_line() + 
  scale_x_date(date_breaks = "1 month",
               labels= date_format("%b-%Y"),
               limits = as.Date(c('2016-02-08','2016-05-08')))

ggplot(fullset, aes(Date, COV_PERC, color = Uimps)) + 
  geom_line() +
  geom_point(aes(size = TOTAL_SALES), alpha = 0.8) + 
  geom_smooth(color = 'dark blue', size = 1.5, method = "lm", formula = y ~ poly(x, 4), se = T, level = 0.68) +
  facet_grid(. ~ DOW) +
  scale_x_date(date_breaks = "1 month",
               labels= date_format("%B"),
               limits = as.Date(c('2016-02-08','2016-05-01'))) +
  ylab("Coverage Percentage") +
  theme_bw() +
  scale_color_gradient(low = 'red', high = 'black')

# can't use par(mfrow = c(1,2)), use gridExtra::grid.arrange()

#MF_SEM series
grid.arrange(
  # 1,1 Uimps ~ Nonimps
  ggplot(fullset, aes(Nonimps, COV_PERC)) + 
    ggtitle("MF SEM Imp Series") +
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)) +
    theme(legend.position = "none"),
  # 1,2 COV_ on Nonimps
  ggplot(fullset, aes(Nonimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)) +
    theme(legend.position = "none"),
  # 1,3 TRANS on Nonimps
  ggplot(fullset, aes(Nonimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)),
  #2,1 Uimps ~ Nonimps
  ggplot(fullset, aes(rs_MFSEMimps, Uimps)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)) +
    theme(legend.position = "none"),
  #2,2 COV_ on Nonimps
  ggplot(fullset, aes(Nonimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)) +
    theme(legend.position = "none"),
  # 2,3 TRANS on Nonimps
  ggplot(fullset, aes(Nonimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)),
  ncol = 3)


#MF_SEM series
grid.arrange(
  # 1,1 Uimps ~ MF_SEM_imps
  ggplot(fullset, aes(MF_SEM_imps, Uimps)) + 
    ggtitle("MF SEM Imp Series") +
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$MF_SEM_imps)) +
    theme(legend.position = "none"),
  # 1,2 COV_ on MF_SEM_imps
  ggplot(fullset, aes(MF_SEM_imps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$MF_SEM_imps)) +
    theme(legend.position = "none"),
  # 1,3 TRANS on MF_SEM_imps
  ggplot(fullset, aes(MF_SEM_imps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$MF_SEM_imps)),
  #2,1 Uimps ~ rs_MF_SEM_imps
  ggplot(fullset, aes(rs_MFSEMimps, Uimps)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_MFSEMimps)) +
    theme(legend.position = "none"),
  #2,2 COV_ on rs_MF_SEM_imps
  ggplot(fullset, aes(rs_MFSEMimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_MFSEMimps)) +
    theme(legend.position = "none"),
  # 2,3 TRANS on rs_MFSEMimps
  ggplot(fullset, aes(rs_MFSEMimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_MFSEMimps)),
  ncol = 3)

# MF_disp_imps series
grid.arrange(
  # 1,1 Uimps ~ MF_disp_imps
  ggplot(fullset, aes(MF_disp_imps, Uimps)) + 
    ggtitle("MF Display Imp Series") +
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$MF_disp_imps)) +
    theme(legend.position = "none"),
  # 1,2 COV_ on MF_disp_imps
  ggplot(fullset, aes(MF_disp_imps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$MF_disp_imps)) +
    theme(legend.position = "none"),
  # 1,3 TRANS on MF_disp_imps
  ggplot(fullset, aes(MF_disp_imps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$MF_disp_imps)),
  #2,1 Uimps ~ rs_MFdispimps
  ggplot(fullset, aes(rs_MFdispimps, Uimps)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_MFdispimps)) +
    theme(legend.position = "none"),
  #2,2 COV_ on rs_MFdispimps
  ggplot(fullset, aes(rs_MFdispimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_MFdispimps)) +
    theme(legend.position = "none"),
  # 2,3 TRANS on rs_MFdispimps
  ggplot(fullset, aes(rs_MFdispimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_MFdispimps)),
  ncol = 3)

# CPA_imps series
grid.arrange(
  # 1,1 Uimps ~ CPA_imps
  ggplot(fullset, aes(CPA_imps, Uimps)) + 
    ggtitle("CPA Imp Series") +
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$CPA_imps)) +
    theme(legend.position = "none"),
  # 1,2 COV_ on CPA_imps
  ggplot(fullset, aes(CPA_imps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$CPA_imps)) +
    theme(legend.position = "none"),
  # 1,3 TRANS on CPA_imps
  ggplot(fullset, aes(CPA_imps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$CPA_imps)),
  #2,1 Uimps ~ rs_CPAimps
  ggplot(fullset, aes(rs_CPAimps, Uimps)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_CPAimps)) +
    theme(legend.position = "none"),
  #2,2 COV_ on rs_CPAimps
  ggplot(fullset, aes(rs_CPAimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_CPAimps)) +
    theme(legend.position = "none"),
  # 2,3 TRANS on rs_CPAimps
  ggplot(fullset, aes(rs_CPAimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_CPAimps)),
  ncol = 3)

# SEM_imps series
grid.arrange(
  # 1,1 Uimps ~ SEM_imps
  ggplot(fullset, aes(SEM_imps, Uimps)) + 
    ggtitle("SEM Imp Series") +
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$SEM_imps)) +
    theme(legend.position = "none"),
  # 1,2 COV_ on SEM_imps
  ggplot(fullset, aes(SEM_imps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$SEM_imps)) +
    theme(legend.position = "none"),
  # 1,3 TRANS on SEM_imps
  ggplot(fullset, aes(SEM_imps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$SEM_imps)),
  #2,1 Uimps ~ rs_SEMimps
  ggplot(fullset, aes(rs_SEMimps, Uimps)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_SEMimps)) +
    theme(legend.position = "none"),
  #2,2 COV_ on rs_SEMimps
  ggplot(fullset, aes(rs_SEMimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_SEMimps)) +
    theme(legend.position = "none"),
  # 2,3 TRANS on rs_SEMimps
  ggplot(fullset, aes(rs_SEMimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_SEMimps)),
  ncol = 3)

# Nonimps series
grid.arrange(
  # 1,1 Uimps ~ Nonimps
  ggplot(fullset, aes(Nonimps, Uimps)) + 
    ggtitle("Total Non-Unique Imp Series") +
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)) +
    theme(legend.position = "none"),
  # 1,2 COV_ on Nonimps
  ggplot(fullset, aes(Nonimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)) +
    theme(legend.position = "none"),
  # 1,3 TRANS on Nonimps
  ggplot(fullset, aes(Nonimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$Nonimps)),
  #2,1 Uimps ~ rs_nonimps
  ggplot(fullset, aes(rs_nonimps, Uimps)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_nonimps)) +
    theme(legend.position = "none"),
  #2,2 COV_ on rs_nonimps
  ggplot(fullset, aes(rs_nonimps, COV_PERC)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_nonimps)) +
    theme(legend.position = "none"),
  # 2,3 TRANS on rs_nonimps
  ggplot(fullset, aes(rs_nonimps, TRANS)) + 
    geom_path(aes(color = as.integer(Date)), size = 1.5) + 
    scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                          low="red", high="blue") +
    xlim(range(fullset$rs_nonimps)),
  ncol = 3)

# frequency
ggplot(fullset, aes(rs_SEMimps, Uimps)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                        low="red", high="blue")

ggplot(fullset, aes(rs_CPAimps, Uimps)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                        low="red", high="blue")

ggplot(fullset, aes(rs_MFdispimps, Uimps)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                        low="red", high="blue")

ggplot(fullset, aes(rs_MFSEMimps, Uimps)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-08"))),
                        low="red", high="blue")

# sales / freq?
ggplot(trimmed_st, aes(rs_nonimps, Sales)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-10"))),
                        low="red", high="blue")
ggplot(trimmed_st, aes(rs_SEMimps, Sales)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-10"))),
                        low="red", high="blue")
ggplot(trimmed_st, aes(rs_CPAimps, Sales)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-10"))),
                        low="red", high="blue")
ggplot(trimmed_st, aes(rs_MFdispimps, Sales)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-10"))),
                        low="red", high="blue")
ggplot(trimmed_st, aes(rs_MFSEMimps, Sales)) + 
  geom_path(aes(color = as.integer(Date)), size = 1.5) + 
  scale_colour_gradient(limits=as.integer(as.Date(c("2016-02-08","2016-05-10"))),
                        low="red", high="blue")

# log/log elasticity
fullset$rs_nonimps2 <- fullset$rs_nonimps^2
summary(lm(Uimps ~ rs_nonimps + rs_nonimps2, fullset) )

ggplot(fullset, aes(log(Nonimps), log(Uimps))) + 
  geom_point() + 
  scale_x_continuous(limits = c(15,16.5)) + 
  scale_y_continuous(limits = c(17,18.5)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x,2))


