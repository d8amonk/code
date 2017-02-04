file.path <- "c:/Users/jvangeete/Desktop/Reports/GMcsv.csv"
gm <- read.csv(file.path, header = T, stringsAsFactors = F)
symbols <- colnames(gm)

symbols
filtered_symbols <- function(symbols) {
  # Name: filter_symbols
  # Purpose: Convert to uppercase if not
  # and remove any non-valid symbols
  # Input: symbols = vector of stock tickers
  # Output: filtered_symbols = filter symbols
  
  # Convert symbols to uppercase
  symbols <- toupper(symbols)
  
  valid <- regexpr("^[A-Z]{2,4}$", symbols)
  
  # Return only the valid ones
  return(sort(symbols[valid == 1]))
}

# filtered_symbols(c("MOT","cvx","123","Gog2","XLe"))

extract_prices <- function(filtered_symbols, file_path){
  # Name: extract_prices
  # Purpose: Read price data from specified file
  # Input: filtered_symbols = vector of stock tickers
  #         file_path = location of price data
  # Output:  prices = data.frame of prices per symbol
  
  # Read in the csv file
  all_prices <- read.csv(file = file_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Make the dates row names
  rownames(all_prices) <- all_prices$Date 
  
  # Remove the original Date column
  all_prices$Date <- NULL
  
  # Extract only the relevant data columns
  valid_columns <- colnames(all_prices) %in% filtered_symbols
  return(all_prices[, valid_columns])
}

filter_prices <- function(prices){
  # Name: filter_prices
  # Purpose: Identify the rows with missing values
  # Input: prices = data.frame of prices
  # Output: missing_rows = vector of indices where
  # data is missing in any of the columns
  
# Returns a boolean vector of good or bad rows
  valid_rows <- complete.cases(prices)
  
  # Identify the index of the missing rows
  missing_rows <- which(valid_rows == FALSE)
  
  return(missing_rows)
}

compute_pairwise_correlations <- function(prices){
  # Name: compute_pairwise_correlations
  # Purpose: Calculates the pairwise corrleations
  # of returns and plots the pairwaise relationships
  # Input: prices = data.frame of prices
  # Output: correlation_matrix = A correlation matrix
  
  # Convert prices to returns
  returns <- apply(prices, 2, function(x) diff(log(x)))
  
  # Plot all the pairwise relationships
  pairs(returns, main = "Pairwise return scatter plot")
  
  # Compute the pairwise correlations
  correlation_matrix <- cor(returns, use = "complete.obs")
  
  return(correlation_matrix)
}



  
  