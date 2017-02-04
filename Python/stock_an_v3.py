import urllib2
import time
import datetime


stocksToPull = 'AAPL','GOOG'#,'MSFT','CMG','AMZN','EBAY','TSLA' #future V should have list, dict passing

def pullData(stock):
	try:
		print 'Currently reading',stock
		print str(datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S'))
		urlToVisit = 'http://chartapi.finance.yahoo.com/instrument/1.0/'+stock+'/chartdata;type=quote;range=1y/csv'

		saveFileLine = stock+'.txt'

		try:
			readExistingData = open(saveFileLine, 'r').read()
			splitExisting = readExistingData.split('\n')
			mostRecentLine = splitExistingp[-2]
			lastUnix = mostRecentLine.split(',')[0]
		except Exception,e:
			print str(e)
			time.sleep(1)
			lastUnix = 0

		saveFile = open(saveFileLine,'a')
		sourceCode = urllib2.urlopen(urlToVisit).read()
		splitSource = sourceCode.split('\n')

		for eachLine in splitSource:
			splitLine = eachLine.split(',')
			if len(splitLine)==6:
				if splitLine[0] > lastUnix:
					if 'values' not in eachLine:
						lineToWrite = eachLine+'\n'
						saveFile.write(lineToWrite)

		saveFile.close()
		
		print 'Pulled', stock
		print '...sleeping...'
		print str(datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S'))
		time.sleep(1)

	except Exception, e:
		print 'main loop', str(e)

for eachStock in stocksToPull:
	pullData(eachStock)