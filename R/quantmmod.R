require(quantmod)
require(TTR)

getSymbols("YHOO", src="google") # from google finance
getSymbols("ATVI", src="google") # from google finance
getSymbols("GOOG", src="yahoo") # from yahoo finance
getSymbols("DEXUSJP", src="FRED") # FX rates from FRED
getSymbols("XPT/USD", src="Oanda") # Platinum from Oanda Sort of handy, but it gets better... > # Specify lookup parameters, and save for future sessions.

setSymbolLookup(YHOO='google', GOOG='yahoo')
setSymbolLookup(DEXUSJP='FRED')
setSymbolLookup(XPTUSD=list(name="XPT/USD", src="oanda"))

saveSymbolLookup(file="mysymbols.rda")

# new sessions call loadSymbolLookup(file="mysymbols.rda")
getSymbols(c("YHOO", "GOOG", "DEXUSJP", "XPTUSD")) 

getSymbols("XPT/USD", src="oanda") 
chartSeries(XPTUSD, name="Platinum (.oz) in $USD") 

chartSeries(to.weekly(ATVI['2014::']), up.col='white', dn.col='blue') 
addMACD()
addBBands()
chartSeries(last(to.daily(ATVI), '26 weeks'),
            up.col = 'white', dn.col = 'blue')
addMACD(2,5,10)
addBBands(n = 20, sd = 3, 
         maType = "EMA", draw = 'bands', on = -1)

chartSeries(Delt(Cl(ATVI), k = 1:3)) #One period lag of the close 
Lag(Cl(ATVI), c(1, 3, 5)) #One, three, and five period lags 
Next(OpCl(ATVI)) #The next periods open to close - today! 
 
# Open to close one-day, two-day and three-day lags
getSymbols("GS")
chartSeries(Delt(Op(ATVI), Cl(ATVI), k=1:3))
# plot.xts(Op(ATVI),Cl(ATVI)) # doesn't work - why?

chartSeries(GS['2013::'], theme="white") #draw the chart 
addVo() #add volume 
addBBands() #add Bollinger Bands 
addCCI() #add Commodity Channel Index
