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

libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("scales","dplyr", "ggplot2", "ggthemes", "data.table"))
rm(libraries)
options(scipen=999)

# /Data
setwd("C:/Users/vangeetej/Google Drive/CDOT/Data/Jason Pulls/") 
setwd("C:/Users/Jeffrey/Google Drive/CDOT/Data/Jason Pulls/") 

# /Ermias
setwd("C:/Users/vangeetej/Google Drive/CDOT/Ermias/FA_MCR/data") 
setwd("C:/Users/Jeffrey/Google Drive/CDOT/Ermias/FA_MCR/data") 

a <- tbl_dt(read.csv("deduped_FAMCR.csv"))
b <- tbl_dt(read.csv("bids_allwin_weng.csv"))

#Build a temp_Date object using the date features of each contract
a$Date <- as.Date(a$Date, format="%m/%d/%Y")
b$awarddate <- as.Date(b$awarddate, format="%m/%d/%Y")
b$lettingdate <- as.Date(b$lettingdate, format="%m/%d/%Y")
a$ID <- as.character(a$ID)
b$ID <- as.character(b$ID)
# a <- subset(a, select = -c(Est_Num, Dt, Day, Unit_Price, Quantity))

# check for NAs (should return a "0-length row.names" object)
summary(a[!complete.cases(a),])
count(a[!complete.cases(a),])
summary(b[!complete.cases(b),])
b <- (b[complete.cases(b),]) #2431+164 = 2595

#Check before moving on... 
glimpse(a)
summary(a)
glimpse(b)
summary(b)

# note any odd grouping?
sort(table(a$Total), decreasing = TRUE)[1:3]

plot(a$Date, a$Total)
plot(density(a$Total[a$Total < 150000 & a$Total > -150000]))

# /////////////////////////////////////
# ////     FILTER & TRAINING AREA   ///
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
         num_lines = n()) %>% 
  group_by(ID) %>% 
  select(Region, Work_Type, num_lines, start, start_year, start_month, end, len_months, Total) %>% 
  mutate(sum_total = sum(Total))
# ,tot = sum(Total))

db <- distinct(select(b, ID, engineersestimate))
da <- distinct(select(c, ID, Region, num_lines, start, start_year, start_month, end, len_months, sum_total))


mdb <- left_join(da, db, by = "ID") %>% unique()
mdb_cc <- mdb[complete.cases(mdb),] 

mdb_cc$engineersestimate <- as.numeric(levels(mdb_cc$engineersestimate)[mdb_cc$engineersestimate])

mab <- mdb_cc[complete.cases(mdb_cc),] %>% 
  mutate(prct = percent(sum_total/engineersestimate))

plot(mab$Date, mab$sum_total)

g <- ggplot(mab, aes(x = start, y = prct)) 
g + geom_line()



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
