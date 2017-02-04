import urllib2
import time
import datetime #error handling best practice




stockToPull = 'AAPL','GOOG','MSFT','CMG','AMZN','EBAY','TSLA','NFLX'
#future V should have list, dict passing
def pullData(stock):
	try:
		fileLine = stock+'.txt'
		#PASTE YAHOO STOCK API URL BELOW, NOTE: RANGE DETERMINES GRANULARITY (DON'T DO '100 months')
		urlToVisit = 'http://chartapi.finance.yahoo.com/instrument/1.0/'+stock+'/chartdata;type=quote;range=1y/csv'
 		sourceCode = urllib2.urlopen(urlToVisit).read()
 		splitSource = sourceCode.split('\n')

 		for eachLine in splitSource:
 			splitLine = eachLine.split(',')
 			#inspect the data, we don't want the str len(6), just the float len(6)
 			if len(splitLine) ==6:
 				if 'values' not in eachLine:
 					saveFile = open(fileLine, 'a') #intent to append
 					lineToWrite = eachLine +'\n'
 					saveFile.write(lineToWrite)
 		print 'Pulled',stock
 		print '...sleeping...'
 		time.sleep(1)

	except Exception, e: #error handle
		print 'main loop', str(e)

#eg just uses one stock, so really it isn't a variable but a parameter
#pullData('AAPL')
#but we are going to use the variable stockToPull bc can define as = [list],{dict}, etc

for eachStock in stockToPull:
	pullData(eachStock)
