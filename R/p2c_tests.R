# n is of uniformly random length between 1 and 10
# take a random sample with replacement of length n from letters between a and j
# concatenate that sample into a string
# store the string as a "path"
# get top 10 how many times that path appears in 400k repetitions of this process

require(dplyr)
require(data.table)
# 
# 
# set.seed(1984)
test <- function(paths = 1000){
  # This section is generating random data - goes away when live. 
  x <- matrix(rep(NA, paths*2), ncol = 2, 
              dimnames = list(c(), c("Cookie", "Site")))
  for(i in 1:paths){
    x[i, 1] <- round(rnorm(1,50000,500))
    n <- function(){sample(1:5, size = 1)}
    draws <- function(){sample(LETTERS[1:20], n(), replace = T)}
    x[i, 2] <- paste(draws(), collapse = '-')
    
    # Draw Dates
    # date_draws <- function(N, st="2016-03-01", et="2016-04-01") {
    #   st <- as.POSIXct(as.Date(st))
    #   et <- as.POSIXct(as.Date(et))
    #   dt <- as.numeric(difftime(et,st,unit="day"))
    #   ev <- sort(runif(N, 0, dt))
    #   rt <- st + ev
    #   }
    
    
    }
  
  x <- as.data.table(x)[, Count := as.character(sum(nchar(gsub("-", "", Site)))) , 
                                by = Cookie]
  x[, Full_path := toString(Site, collapse = '-'), by = Cookie]
  
  
  
  return(x)

  }