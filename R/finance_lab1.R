# Assign the url to the csv file
data_url = "http://faculty.washington.edu/ezivot/econ424/sbuxPrices.csv"
sbux_df = read.csv(file = data_url, header = TRUE, stringsAsFactors = FALSE)
# sbux_df is a data frame object. Data frames are rectangular data objects
# typically with observations in rows and variables in columns

closing_prices = sbux_df[,"Adj.Close",drop=FALSE]
# Find indices associated with the dates 3/1/1994 and 3/1/1995
index_1 = which(sbux_df$Date=="3/1/1994")
index_2 = which(sbux_df$Date=="3/1/1995")
# Extract prices between 3/1/1994 and 3/1/1995
some_prices = sbux_df[index_1:index_2,"Adj.Close"]

# Create a new data frame containing the price data with the dates as the row names
sbux_prices_df = sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) = sbux_df$Date
head(sbux_prices_df)
# With Dates as rownames, you can subset directly on the dates.
# Find indices associated with the dates 3/1/1994 and 3/1/1995.
price_1 = sbux_prices_df["3/1/1994",1]
price_2 = sbux_prices_df["3/1/1995",1]

#plot the price data
plot(sbux_df$Adj.Close, type = "l", col = "blue", lwd = 2, ylab = "Adjusted close", 
    main = "Monthly closing price of SBUX")

# Denote n the number of time periods IOT calculate simple returns
n = nrow(sbux_prices_df)
sbux_ret = (sbux_prices_df[2:n,1] - sbux_prices_df[1:(n-1),1])/sbux_prices_df[1:(n-1),1]
# Notice that sbux_ret is not a data frame object
class(sbux_ret)
plot(sbux_ret, type = "l", col = "blue", lwd = 2, ylab = "Adjusted close", main = "Monthly closing price of SBUX")

# Compute continuously compounded 1-month returns
sbux_ccret = log(sbux_prices_df[2:n, 1])-log(sbux_prices_df[1:(n-1), 1])
# Assign names to the continuously compounded 1-month returns
names(sbux_ccret) = sbux_df[2:n,1]
# Show sbux_ccret and compare compounding returns to simple returns
# NOTE: compounding returns are always smaller
head(sbux_ccret)
both_ret=cbind(sbux_ret,sbux_ccret)
head(both_ret)

#graphical comparison of continuous and discrete returns
plot(sbux_ret, type = "l", col = "blue", lwd = 2, ylab = "Return", main = "Monthly Returns on SBUX")
# Add horizontal line at zero
abline(h = 0)
# Add a legend
legend(x = "bottomright", legend = c("Simple", "CC"), lty = 1, lwd = 2, col = c("blue", "red"))
# Add the continuously compounded returns
lines(sbux_ccret, col="red",lwd=2)
