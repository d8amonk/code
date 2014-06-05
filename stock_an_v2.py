import urllib2
import time 
import datetime #error handling best practice, video #sentdex.4 

stocksToPull = 'AAPL'
#,'GOOG','MSFT','CMG','AMZN','EBAY','TSLA','NFLX' #future V should have list, dict passing

def pullData(stock):
	try:
		print 'Currently pulling', stock
		print  str(datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S'))
		urlToVisit = 'http://chartapi.finance.yahoo.com/instrument/1.0/'+stock+'/chartdata;type=quote;range=1y/csv'
		saveFileLine = stock+'.txt'
		
		try:
			readExistingData = open(saveFileLine,'r').read() #intention to read
			splitExisting = readExistingData.split('\n')
			mostRecentline = splitExisting[-2] #last line ([-1] is blank)
			lastUnix = mostRecentline.split(',')[0]  #will read UNIX stamp as int, but *
		except Exception, e:
			print str(e)
			time.sleep(1)
			lastUnix = 0 #if the file above doesn't exist, take the whole thing (we want it)

		saveFile = open(saveFileLine,'a')
		sourceCode = urllib2.urlopen(urlToVisit).read()
		splitSource = sourceCode.split('\n')

		for eachLine in splitSource:
			if 'values' not in eachLine:
				splitLine = eachLine.split(',')
				if len(splitLine) == 6:
					if int(splitLine[0]) > int(lastUnix): #if the first line to write is newer than the last line in the WRITTEN file, then append \n
						
						lineToWrite = eachLine+'\n' 
						saveFile.write(lineToWrite)
		saveFile.close() #ow file isn't immediately accessible

		print 'Pulled', stock
		print '...zzz...'
		print  str(datetime.datetime.fromtimestamp(time.time()).strftime('%Y-%m-%d %H:%M:%S'))
		time.sleep(1)

	except Exception, e: #error handle
		print 'main loop', str(e)

for eachStock in stocksToPull:	#eg just uses one stock, so really it isn't a variable but a parameter
	pullData(eachStock)			#pullData('AAPL')
								#but we are going to use the variable stockToPull bc can define as = [list],{dict}, etc













