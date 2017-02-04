# incentive pays (+/-)
# - contingency = force account
# - on average == 10%, only spending 3%
#
# gradation acceptance rate - does this affect the incetive %pp?
#
# wahta re the differences in the distributions around mean = 10
#
# all non-biddable items n(Contrs don't bid on their own pays!)
#

libraries <- function(x) 
  sapply(x, require, character.only=T) 


libraries(list("gridExtra","data.table", "scales","dplyr", "ggplot2", "ggthemes"))
rm(libraries)
options(scipen=999, digits = 3)

# /Data
setwd("C:/Users/vangeetej/Google Drive/CDOT/Data/Jason Pulls/") 
setwd("C:/Users/Jeffrey/Google Drive/CDOT/Data/Jason Pulls/") 

# /Ermias
setwd("C:/Users/vangeetej/Google Drive/CDOT/DAF/FA_MCR/data") 
setwd("C:/Users/Jeffrey/Google Drive/CDOT/DAF/FA_MCR/data") 

a <- tbl_dt(read.csv("deduped_FAMCR.csv"))
b <- tbl_dt(read.csv("bids_allwin_weng.csv"))

#Build a temp_Date object using the date features of each contract
a$Date <- as.Date(a$Date, format="%m/%d/%Y")
b$awarddate <- as.Date(b$awarddate, format="%m/%d/%Y")
b$lettingdate <- as.Date(b$lettingdate, format="%m/%d/%Y")
a$ID <- as.character(a$ID) #FA/MCR data pull AO 9jul
b$ID <- as.character(b$ID) #engineersestimate and awardamount from bids analysis 
# (method: matching / merging / joining, key = ID)

# x <- subset(a, select = -c(Est_Num, Dt, Day, Unit_Price, Quantity))

# Due diligence
summary(a[!complete.cases(a),])
summary(b[!complete.cases(b),])
glimpse(a)
summary(a)
glimpse(b)
summary(b)

# note any odd grouping?
# sort(table(a$Total), decreasing = TRUE)[1:3]
# 
# plot(a$Date, a$Total)
# plot(density(a$Total[a$Total < 150000 & a$Total > -150000]))

# /////////////////////////////////////
# /////////////////////////////////////
# /////////////////////////////////////
# ////                              ///
# ////     FILTER & TRAINING AREA   ///
# ////                              ///
# /////////////////////////////////////
# /////////////////////////////////////
# /////////////////////////////////////
# Using dplyr: build contract length, num payments, and total payments 
# (Replaces pivot sections)
  
c <- a %>% 
  group_by(ID) %>% 
  mutate(date = Date,
         start = as.Date(min(Date)),
         start_year = year(start),
         start_month = month(start),
         end = as.Date(max(Date)),
         len = as.numeric(difftime(end, start), unit="days"),
         len_months = ceiling(len/30),
         num_lines = n()) #getting counts of line items, lengths in days+months, various feature-factors like month/year

db <- distinct(select(b, ID, engineersestimate, awardamount))
da <- distinct(select(c, ID, Work_Type, Region, num_lines, start, start_year, start_month, end, len_months, Total))

ga <- da %>% 
  group_by(ID) %>% 
  mutate(sum_total = sum(Total))

mdb <- left_join(ga, db, by = "ID") %>% unique()
mdb$awardamount <- as.numeric(levels(mdb$awardamount))[mdb$awardamount]
mdb$engineersestimate <- as.numeric(levels(mdb$engineersestimate))[mdb$engineersestimate]
mdb$Region <- as.factor(mdb$Region)
mdb <- mdb[complete.cases(mdb),] 

mdb <- mdb %>% #BREAK IF !== 320 OBS OF 15 FEATS
  mutate(eng_prct = 100*sum_total/engineersestimate,
         awd_prct = 100*sum_total/awardamount)

# a look w/o outliers
mo <- mdb[mdb$awd_prct > quantile(mdb$awd_prct, .25) - 1.5*IQR(mdb$awd_prct) &
            mdb$awd_prct < quantile(mdb$awd_prct, .75) + 1.5*IQR(mdb$awd_prct)]


# CAREFUL! typeof == char!
# mdb$eng_prct <- percent(mdb$eng_prct)
windowsFonts(Times=windowsFont("TT Times New Roman"))

g0 <- ggplot(mdb[mdb$Region == 0], aes(x = awd_prct)) + 
  geom_histogram(binwidth = 2.5) +
  theme(axis.title.x = element_blank(), axis.title.y = element_text("Region HQ"))
g1 <- ggplot(mdb[mdb$Region == 1], aes(x = awd_prct)) + 
  geom_histogram(binwidth = 2.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text("Region 1"))
g2 <- ggplot(mdb[mdb$Region == 2], aes(x = awd_prct)) + 
  geom_histogram(binwidth = 2.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text("Region 2"))
g3 <- ggplot(mdb[mdb$Region == 3], aes(x = awd_prct)) + 
  geom_histogram(binwidth = 2.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text("Region 3"))
g4 <- ggplot(mdb[mdb$Region == 4], aes(x = awd_prct)) + 
  geom_histogram(binwidth = 2.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text("Region 4"))
g5 <- ggplot(mdb[mdb$Region == 5], aes(x = awd_prct)) + 
  geom_histogram(binwidth = 2.5) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_text("Region 5"))

gA <- ggplot(mdb, aes(x = awd_prct)) + 
  geom_histogram(aes(y = ..density.., alpha = 0.1, fill = Region))

grid.arrange(g0,g1,g2,g3,g4,g5, ncol = 3,
             main = "FA/MCR Item Totals as % of Award Amount",
             sub = "N = 320, Y = 2011-15")

g0 <- ggplot(mdb[mdb$Region == 0], aes(x = awd_prct)) + 
  geom_density(aes(y =..density..)) +
  theme_minimal() +
  theme(axis.title.x = element_blank())
g1 <- ggplot(mdb[mdb$Region == 1], aes(x = awd_prct)) + 
  geom_density(aes(y =..density..)) +
  theme_minimal() +
  theme(axis.title.x = element_blank())
g2 <- ggplot(mdb[mdb$Region == 2], aes(x = awd_prct)) + 
  geom_density(aes(y =..density..)) +
  theme_minimal() +
  theme(axis.title.x = element_blank())
g3 <- ggplot(mdb[mdb$Region == 3], aes(x = awd_prct)) + 
  geom_density(aes(y =..density..)) +
  theme_minimal() +
  theme(axis.title.x = element_blank())
g4 <- ggplot(mdb[mdb$Region == 4], aes(x = awd_prct)) + 
  geom_density(aes(y =..density..)) +
  theme_minimal() +
  theme(axis.title.x = element_blank())
g5 <- ggplot(mdb[mdb$Region == 5], aes(x = awd_prct)) + 
  geom_density(aes(y =..density..)) +
  theme_minimal() +
  theme(axis.title.x = element_blank())

gA <- ggplot(mdb, aes(x = awd_prct)) + 
  geom_density(aes(y = ..density.., alpha = 0.1, fill = Region))

# par(mfrow = c(6,1))
grid.arrange(g0,g1,g2,g3,g4,g5, ncol = 1, widths = c(0.3))

# grid.arrange(g0, g1,g2,g3,g4,g5, widths=c(0.6, 0.3), ncol=2)

grid.arrange(g0, g1, heights=c(0.7, 0.3), nrow=2)
# for spacing between plots:
  
blank<-rectGrob(gp=gpar(col="white")) # make a white spacer grob
grid.arrange(g0, blank, g1, heights=c(0.7, 0.05, 0.25), nrow=3)

grid.newpage()
grid.draw(rbind(ggplotGrob(g0), ggplotGrob(g1), ggplotGrob(g2), ggplotGrob(g3), ggplotGrob(g4), ggplotGrob(g5), size = "last"))

# GROUP BY ITEM DESCRIPTION!
# bmi  <- rnorm(500, 23,2)
# summary(bmi)
# t.test(bmi, mu = 22.5)
# args(plot.default)
# pain  <- c(0,1,3,3,2,1,0,2,2,1,3,1)
# length(pain)
# pain  <- subset(pain, select = pain[1:10])
# pain[1:10]
# pain  <- pain[1:10]
# length(pain)
# pain
# fpain  <- factor(pain, levels = 0:3)
# levels(fpain)  <- c("none","low","med","high")
# fpain
# as.numeric(fpain)
# levels(fpain)
# summary(fpain)
# plot(fpain)
# plot(density(fpain))

plot(jitter(Carbon) ~ jitter(City),
     xlab = "City MPG", 
     ylab = "Carbon footprint (tonnes per year)")

fit <- lm(Carbon ~ City, fuel)
abline(fit)

summary(fit)
