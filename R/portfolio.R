library(quantmod)
library(dplyr)
# library(data.table)? # How would this benefit us?

NVDA <- 
  getSymbols('NVDA', auto.assign = FALSE)['2015::']
FCEL <- 
  getSymbols('FCEL', auto.assign = FALSE)['2015::']
ATVI <- 
  getSymbols('ATVI', auto.assign = FALSE)['2015::']
FSLR <- 
  getSymbols('FSLR', auto.assign = FALSE)['2015::']
NFLX <- 
  getSymbols('NFLX', auto.assign = FALSE)['2015::']
OPTT <- 
  getSymbols('OPTT', auto.assign = FALSE)['2015::']
PLUG <- 
  getSymbols('PLUG', auto.assign = FALSE)['2015::']
portfolio <- list(NVDA, FCEL, ATVI, FSLR, NFLX, OPTT, PLUG)

apply.weekly(FSLR, FUN = function(x) {max(Cl(x))})
# faster to use endpoints(data, on = 'weeks')
period.apply(OPTT, endpoints(OPTT, on = 'weeks'),
             FUN = function(x) {max(Cl(x))})
# even faster to use
period.max(Cl(OPTT), endpoints(OPTT, on = 'weeks'))
# remember period.min(), period.prod(), period.sum()

sum(dailyReturn(NVDA))
summary(dailyReturn(NVDA))
var(dailyReturn(NVDA))

sum(weeklyReturn(NVDA))
summary(weeklyReturn(NVDA))
var(weeklyReturn(NVDA))

sum(monthlyReturn(NVDA))
summary(monthlyReturn(NVDA))
var(monthlyReturn(NVDA))

# plot(allReturns(NVDA)) # doesn't work (needs univariate)
getSymbols.FRED('CFNAI', env=globalenv())


