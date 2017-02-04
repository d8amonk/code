cw <- Quandl("FRED/COWCON")
ce <- Quandl("FRED/COCONS")
cw$Value  <- log(cw$Value)
ce$Value  <- log(ce$Value)

## first some dummy data
set.seed(12345)
dat <- data.frame(price = cumsum(rnorm(450)),
                  date = seq(from = as.Date("2008-01-01"),
                             length = 450, by = "days"))

## get the months of observations
dat$month <- factor(format(dat$date, format = "%b"), levels = month.abb)

## and then format this for the quarters
dat$quarter <- character(length = NROW(dat))
## I'm sure these 4 steps can be simplified but just
## how escapes me at the moment
dat$quarter[dat$month %in% month.abb[c(12,1,2)]] <- "Winter"
dat$quarter[dat$month %in% month.abb[c(3:5)]] <- "Spring"
dat$quarter[dat$month %in% month.abb[c(6:8)]] <- "Summer"
dat$quarter[dat$month %in% month.abb[c(9:11)]] <- "Autumn"
dat$quarter <- factor(dat$quarter, 
                      levels = c("Spring","Summer","Autumn","Winter"))

## look at the fruits of our labour
head(dat)

## create period
runs <- rle(as.numeric(dat$quarter))$lengths
dat$period <- factor(rep(seq_along(runs), times = runs))

## aggregate
with(dat, aggregate(price, list(quarter = quarter, period = period), 
                    FUN = mean))

# I use the rle() function (run length encoding) calculate the number of
# observations where the 'quarter' remains the same:
  
rle(as.numeric(dat$quarter))
Run Length Encoding
lengths: int [1:6] 60 92 92 91 90 25
values : num [1:6] 4 1 2 3 4 1

# The 'values' here are the numeric representation of the quarter factor.
# The most interesting for us is the second 4 - this is the winter 2008/9.
# I use the lengths to replicate a period number (1,2,...,n) the correct
# number of times. Now we have the period correctly calculated, we just
# aggregate by quarter and period to give the averages you want.
# 
# If you are working on months 1-3 as quarter 1, 4-6 as quarter 2 etc,
# then it is much easier, just aggregate by quarter and year:
  
  ## copy the data above
  dat2 <- dat
## change meaning of quarter
dat2$quarter <- character(length = NROW(dat2))
dat2$quarter[dat2$month %in% month.abb[c(1:3)]] <- "Q1"
dat2$quarter[dat2$month %in% month.abb[c(4:6)]] <- "Q2"
dat2$quarter[dat2$month %in% month.abb[c(7:9)]] <- "Q3"
dat2$quarter[dat2$month %in% month.abb[c(10:12)]] <- "Q4"
dat2$quarter <- factor(dat2$quarter, levels = c("Q1","Q2","Q3","Q4"))
## year variable
dat2$year <- factor(format(dat2$date, format = "%Y"))

## drop the first 40 days to simulate a late starting record
## and aggregate
with(dat2[-(1:40), ], aggregate(price, list(quarter = quarter, year =
                                              year), FUN = mean))

Which gives:
  quarter year        x
1      Q1 2008 13.58644
2      Q2 2008 24.16523
3      Q3 2008 28.56004
4      Q4 2008 32.60900
5      Q1 2009 44.86594