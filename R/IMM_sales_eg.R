####################################
####################################
####### PRODUCT ####################
####### PROPELLER ##################
####################################
####################################
library(dplyr)

k.stores <- 20 #k number of stores
k.weeks <- 104 #k number of years in wks

#create a data frame of initiailly missing values to hold the data
store.df <- data.frame(matrix(NA, ncol = 10, nrow = k.stores*k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", 
                     "p1sales", "p2sales", 
                     "p1price", "p2price", 
                     "p1prom", "p2prom", 
                     "country")
store.num <- 101:(100+k.stores)
(store.cty <- c(rep("US", 3), rep("DE",5), rep("GB", 3), rep("BR", 2),
                rep("JP", 4), rep("AU", 1), rep("CN",2)))
length(store.cty) #make sure the store.cty vector is the right length

#expand vectors to match the number of stores and weeks
store.df$storeNum <- rep(store.num, each = k.weeks)
store.df$country <- rep(store.cty, each = k.weeks)
rm(store.num, store.cty) #clean up

#do the same for Week and Year cols
(store.df$Week <- rep(1:52, times = k.stores*2))
(store.df$Year <- rep(rep(1:2, each = k.weeks/2), times = k.stores))
str(store.df)

#recode country and storeNum to factor (cat) vars
store.df$country <- as.factor(store.df$country)
store.df$storeNum <- as.factor(store.df$storeNum)

str(store.df)
head(store.df) #head(store.df, 120)
tail(store.df) #tail(store.df, 120)

#fill NAs with PRNG values (using binom distribution = coin toss)
set.seed(80301) #Boulder Zip for the PRNG
set.seed(98250) #Book eg for the PRNG

store.df$p1prom <- rbinom(n=nrow(store.df), size = 1, p = 0.1) #10% P(promotion)
store.df$p2prom <- rbinom(n=nrow(store.df), size = 1, p = 0.15) #15% P(promotion)
head(store.df)
#check the P() came out as expected
library(scales)
percent(mean(store.df$p1prom)); sum(store.df$p1prom) #check line ~110 for mean apply() f
percent(mean(store.df$p2prom)); sum(store.df$p2prom)

#set (multiple, distinct) price points for each product
store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99),
                           size = nrow(store.df), replace = TRUE)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19),
                           size = nrow(store.df), replace = TRUE)
head(store.df) #now how does it look? (NOTE: haven't done sales yet!)

#sales data, using poisson (counts) distribution, rpois()
#first, the default sales in the absence of promotion
tmp.sales1 <- rpois(nrow(store.df), lambda = 120) #mean(tmp.sales1), check ~120
tmp.sales2 <- rpois(nrow(store.df), lambda = 100) #mean(tmp.sales2), check ~100

#scale sales according to relative prices, aka the INVERSE ratio of log(price)
#price effects often follow a log rather than linear function...
tmp.sales1 <- tmp.sales1 * (log(store.df$p2price) / log(store.df$p1price))
tmp.sales2 <- tmp.sales2 * (log(store.df$p1price) / log(store.df$p2price)) #troubleshoot spot

#lets assume sales get a 30% and 40% lift from promotion, respectively
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom * 0.3)) #these coefs will be fed by historicals
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))
head(store.df)
library(car)
some(store.df, 10) #grab a random sample
#so far: 20,800 values in 22 commands

###########################
#### POTENTIALLY BREAK ####
#### SCRIPT HERE ##########
###########################
#####SUM TABLES############
###########################

summary(store.df, digits = 3)
library(psych)
describe(store.df)

#how many times was each product observed to be on sale at a given price point?
(p1.table <- table(store.df$p1price)) #use tables for discrete vars
plot(p1.table)

#how often was each product promoted at each price point?
(p1.table2 <- table(store.df$p1price, store.df$p1prom))

#proprotion of time the product was promoted at each price point
options(scipen = 999, digits = 3)
p1.table2[,2]*100 / (p1.table2[,1] + p1.table2[,2])
IQR(store.df$p1sales)
IQR(store.df$p2sales)
(p1.deciles <- quantile(store.df$p1sales, probs = c(0:10/10)))
(p2.deciles <- quantile(store.df$p2sales, probs = c(0:10/10)))
p1.deciles > p2.deciles

#build a useful summary table (eg median sales and IQR by product)
mysummary.df <- data.frame(matrix(NA, nrow = 2, ncol = 2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

#apply version of
apply(store.df[,4:9], MARGIN = 2, mean)
apply(store.df[,3:9], MARGIN = 2, sum)/52
apply(store.df[,4:9], MARGIN = 2, sd)
apply(store.df[,2:9], MARGIN = 2, mad)

#flag skew in the data
apply(store.df[,2:9], 2, function(x){mean(x) - median(x)})

#standard hist
par(mfrow = c(1,2))
hist(store.df$p1sales,
     main = "Product 1 Weekly Sales Frequencies, All Stores",
     xlab = "Product 1 Sales (Units)",
     ylab = "Relative Frequency",
     breaks = 100, #more columns
     col = "lightblue", #color the bars
     freq = FALSE,
     xaxt = "n")
axis(side = 1, at = seq(60,300, by = 20))
lines(density(store.df$p1sales, bw = 10), #bw adjusts smoothing
      type = 'l', col = 'darkred', lwd = 2)

hist(store.df$p2sales,
     main = "Product 2 Weekly Sales Frequencies, All Stores",
     xlab = "Product 2 Sales (Units)",
     ylab = "Relative Frequency",
     breaks = 100, #more columns
     col = "lightblue", #color the bars
     freq = FALSE,
     xaxt = "n")
axis(side = 1, at = seq(60,300, by = 20))
lines(density(store.df$p2sales, bw = 10), #bw adjusts smoothing
      type = 'l', col = 'darkred', lwd = 2)
dev.off()

# Tukey boxplot, p2sales by storeNum
boxplot(p2sales ~ storeNum, data = store.df, horizontal = TRUE,
        xlab = "Weekly Unit Sales", ylab = "Store", las = 1,
        main = "Weekly Sales of P2 by Store")

boxplot(p2sales ~ p2prom, data = store.df, horizontal = TRUE, yaxt = "n",
        xlab = "Weekly Unit Sales", ylab = "Product Promoted?" , las = 1, 
        main = "Weekly Sales of P2 by Promotion")
axis(side = 2, at = c(1,2), labels = c("No", "Yes"))

# qqplots ID a non-normal dist; log() transforms provide a fix
par(mfrow = c(1,2))
qqnorm(store.df$p1sales, main = "Untransformed QQ")
qqline(store.df$p1sales, col = 'red', lwd = 2) #anden...
qqnorm(log(store.df$p1sales), main = "Transformed QQ")
qqline(log(store.df$p1sales), col = 'red', lwd = 2)
dev.off()

plot(ecdf(store.df$p1sales),
     main = "Cumulative Distribution of P1 Weekly Sales",
     ylab = "Cumulative Proportion",
     xlab = c("P1 Sales, All Stores","90% of weeks sold <= 171 Units"),
     yaxt = "n")
axis(side = 2, at = seq(0, 1, by = 0.1), las = 1,
     labels = paste(seq(0, 100, by = 10), "%", sep = ""))
# moneymaker
abline(h = 0.9, lty = 3) #h = horizontal line, lty = 3 = dotted
abline(v = quantile(store.df$p1sales, pr = 0.9), lty = 3) #v for verticle line

# break out mean sales of P1 by store
by(store.df$p1sales, store.df$storeNum, mean)
(p1sales.sum <- aggregate(store.df$p1sales, by = list(country = store.df$country), sum))

# map it
library(rworldmap)
library(RColorBrewer)

p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2",
                                   nameJoinColumn = "country")
# "7 codes from your data successfully matched countries in the map"
mapCountryData(p1sales.map, nameColumnToPlot = "x",
    mapTitle = "Total P1 Sales by Country",
    colourPalette = brewer.pal(7, "Greens"),
    catMethod = "fixedWidth", addLegend = FALSE)

