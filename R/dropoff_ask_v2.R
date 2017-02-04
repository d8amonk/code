tf <- read.csv("c:/Users/jvangeete/Desktop/tf.csv")
st <- read.csv("c:/Users/jvangeete/Desktop/st.csv")
nt <- read.csv("c:/Users/jvangeete/Desktop/nt.csv")
tw <- read.csv("c:/Users/jvangeete/Desktop/tw.csv")

tf$Date <- as.Date(as.character(tf$Date), format = '%m/%d/%Y')
st$Date <- as.Date(st$Date, format = '%m/%d/%Y')
nt$Date <- as.Date(as.character(nt$Date), format = '%m/%d/%Y')
tw$Date <- as.Date(as.character(tw$Date), format = '%m/%d/%Y')

summary(tf)
summary(st)
summary(nt)
summary(tw)

lm_tf_sales <- lm(Sales ~ Date, tf)
lm_tf_sim <- lm(SIM ~ Date, tf)
lm_tf_dev <- lm(Device ~ Date, tf)

lm_st_sales <- lm(Sales ~ Date, st)
lm_st_sim <- lm(SIM ~ Date, st)
lm_st_dev <- lm(Device ~ Date, st)

lm_nt_sales <- lm(Sales ~ Date, nt)
lm_nt_sim <- lm(SIM ~ Date, nt)
lm_nt_dev <- lm(Device ~ Date, nt)

lm_tw_sales <- lm(Sales ~ Date, tw)
lm_tw_sim <- lm(SIM ~ Date, tw)
lm_tw_dev <- lm(Device ~ Date, tw)

par(mfrow = c(3,1))
plot(tf$Date, tf$Sales, type = 'l')
abline(lm_tf_sales)
plot(tf$Date, tf$SIM, type = 'l')
abline(lm_tf_sim)
plot(tf$Date, tf$Device, type = 'l')
abline(lm_tf_dev)
lm_tf_sales
lm_tf_sim
lm_tf_dev

plot(st$Date, st$Sales, type = 'l')
abline(lm_st_sales)
plot(st$Date, st$SIM, type = 'l')
abline(lm_st_sim)
plot(st$Date, st$Device, type = 'l')
abline(lm_st_dev)
lm_st_sales
lm_st_sim
lm_st_dev

plot(nt$Date, nt$Sales, type = 'l')
abline(lm_nt_sales)
plot(nt$Date, nt$SIM, type = 'l')
abline(lm_nt_sim)
plot(nt$Date, nt$Device, type = 'l')
abline(lm_nt_dev)
lm_nt_sales
lm_nt_sim
lm_nt_dev

plot(tw$Date, tw$Sales, type = 'l')
abline(lm_tw_sales)
plot(tw$Date, tw$SIM, type = 'l')
abline(lm_tw_sim)
plot(tw$Date, tw$Device, type = 'l')
abline(lm_tw_dev)
lm_tw_sales
lm_tw_sim
lm_tw_dev











