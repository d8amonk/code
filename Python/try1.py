import urllib2
import time
import datetime


stocksToPull = 'AAPL','GOOG',#'MSFT','CMG','AMZN','EBAY','TSLA' #future V should have list, dict passing

def pullData(stock):
	try:
		print 'Currently reading',stock
		print str(datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S'))
		urlToVisit = 'http://chartapi.finance.yahoo.com/instrument/1.0/'+stock+'/chartdata;type=quote;range=1y/csv'

		saveFileLine = stock+'.txt'

		try:
			readExistingData = open(saveFileLine, 'r').read()
			splitExisting = readExistingData.split('\n')
			mostRecentLine = splitExisting[-2]
			lastUnix = mostRecentLine.split(',')[0]
		except Exception,e:
			print str(e)
			time.sleep(1)
			lastUnix = 0

		saveFile = open(saveFileLine,'a')
		sourceCode = urllib2.urlopen(urlToVisit).read()
		
		splitSource = sourceCode.split('\n')

		for eachLine in splitSource:
			if 'values' not in eachLine:
				splitLine = eachLine.split(',')
				if len(splitLine)==6:
					if int(splitLine[0]) > int(lastUnix):

						lineToWrite = eachLine+'\n'
						saveFile.write(lineToWrite)

		saveFile.close()
		
		print 'Pulled', stock
		print '...sleeping...'
		print str(datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S'))
		time.sleep(5)  #CHANGE THIS FOR BIG LISTS

	except Exception, e:
		print 'main loop', str(e)

#while True:
	for eachStock in stocksToPull:
		pullData(eachStock)
	#time.sleep(18000) 5h delayed repeat, eg
