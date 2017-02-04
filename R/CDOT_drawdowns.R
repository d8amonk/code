//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
///       ***this is the DRAWDOWN section***       ///
///  THIS SECTION RUNS ON EXP, EXP.c OBJECTS       ///
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////

#####################################
####       UPCOMING TASKS         ###
#####################################

#####################################
####        LOADING AREA          ###
#####################################

# Much libraries
libraries <- function(x) lapply(x, require, character.only=T) 
libraries(c("fpp", "forecast", "lattice", "ggplot2", "dplyr", "timeSeries", "tseries", "xts", "data.table"))
remove(libraries)

# So data
setwd("c:/Users/vangeetej//Google Drive/CDOT/Data/On Deck/") 

h_01_14 <- read.csv("SM_01_16.csv")
# zj_40 <- read.csv("c:/Users/vangeetej/Google Drive/CDOT/Data/Reports Data/ZJ40/Zj40_v1.csv", header = T) 
# str(h_01_14)


#####################################
####   BUILDING & TRAINING AREA   ###
#####################################

# Build a temp_Date object using the date features of each contract
temp_Date_01 <- as.Date(paste(h_01_14$Month,h_01_14$Day,h_01_14$Year,sep="/"), format="%m/%d/%Y")
# str(temp_Date_01)

# Select columns 
h_01_14 <- h_01_14 %>% select(ID, Amount, Month, Day, Year)
# str(h_01_14)
# Check NAs
# summary(h_01_14[!complete.cases(h_01_14),])

# make a data.frame with an ISO date column
exp_01 <- data.frame(h_01_14, Date = temp_Date_01)

# LOOK at the data
# head(exp_01, 10)

# Using dplyr, build views by contract, length, num payments, and total payments 
exp.c <- exp_01 %>% 
  arrange(ID) %>% 
  select(ID,
         Amount,
         Date)

exp <- exp %>% 
  group_by(ID) %>% 
  summarise(start = min(Date),
            end = max(Date),
            len = as.numeric(difftime(last(Date), first(Date), unit="days")),
            len_months = ceiling(len/30), 
            num_payments = n(),
            tot = sum(Amount)) 

exp <- arrange(exp, start, tot) 
# exp <- filter(exp, tot > 10000, tot < 25000000)

# an empty vector to store quantile values in
q <- quantile(exp$tot,c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
str(q)

q1 <- exp %>% select(tot, len_months, num_payments) %>%  filter(0 < tot, tot < q[1]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()                                          
q2 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[1] < tot, tot < q[2]) %>% summarize(n = n(),  avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q3 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[2] < tot, tot < q[3]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q4 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[3] < tot, tot < q[4]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q5 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[4] < tot, tot < q[5]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q6 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[5] < tot, tot < q[6]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q7 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[6] < tot, tot < q[7]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q8 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[7] < tot, tot < q[8]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q9 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[8] < tot, tot < q[9]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()
q10 <- exp %>% select(tot, len_months, num_payments) %>%  filter(q[9] < tot, tot < q[10]) %>% summarize(n = n(), avg_len_months = mean(len_months), avg_tot = mean(tot)) %>% data.frame()

contract_slices = rbind(q1,q2,q3,q4,q5,q6,q7,q8,q9,q10)

cs_sum_temp <- summarise(contract_slices, n = sum(n), avg_len_months = mean(avg_len_months), avg_tot = mean(avg_tot))

meta_contract <- rbind(contract_slices, cs_sum_temp)

rownames(meta_contract) <- c('q1','q2','q3','q4','q5','q6','q7','q8','q9','q10', 'summary')

write.csv(meta_contract, "meta_deciles.csv")

#####################################
####         FITTING AREA         ###
#####################################

lm(Amount ~ Date + len + num_payments + tot, exp.l)
fit <- lm(Amount ~ Date + len + num_payments + tot, exp.l)
plot(fit)


#####################################
####       PLOTTING AREA          ###
#####################################
# comparison of plots with PRFs overlaid
dev.off()
par(mfrow = c(2,1))

plot(exp.c$tot,
     exp.c$len,
     pch = 1,
     xlab = "$$$", 
     ylab = "# Days", 
     main = "Does a contract's total budget drive its length in days?",
     sub = "THIS IS ONLY FOR DEMO PURPOSES")
#col=rgb(1,0,0,.8)), maxcolorValue=255?
abline(lm(len ~ tot, exp.c), col="red", lwd = 2, lty = 1)
plot(exp.c$tot, 
     exp.c$num_payments, 
     xlab = "$$$", 
     ylab = "# of Payments", 
     main = "What about the number of payments we have to make (i.e. does a larger $ contract have more or larger payments)?",
     sub = "THIS IS ONLY FOR DEMO PURPOSES")
abline(lm(num_payments ~ tot, exp.c), col="red", lwd = 2, lty = 1)
#reset graphics device
dev.off()

require(ggplot2)
qplot(exp.l$Amount, geom = 'point')
qplot(exp.l$tot, exp.l$Amount, geom = 'point') + aes(color = exp.l$Month, alpha = 0.5)
qplot(exp.l$len, exp.l$Amount, geom = 'point') + aes(size = exp.l$len_months)
qplot(exp.l$Month, exp.l$Amount)

qplot(as.numeric((exp.l$start)))
qplot(as.numeric(exp.l$end))
qplot(exp.l$len)
qplot(exp.l$num_payments)
qplot(exp.l$Day)

p <- ggplot(exp.l, aes(num_payments, tot))
p + geom_point(size = 3)+ scale_shape_identity()
p + geom_point(aes(color = tot)) + scale_shape_identity()
