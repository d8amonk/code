# import urllib2
# from BeautifulSoup import BeautifulSoup as bs

# page1 = urllib2.urlopen("http://www.wunderground.com/history/airport/DEN/2010/10/1/DailyHistory.html")
# #page2 = urllib2.urlopen("http://www.wunderground.com/history/airport/DEN/2009/1/1/DailyHistory.html")

# soup = bs(page1)

# images = soup.findAll('img')
# first_image = images[0]
# src = first_image['src']
# #we don't want images, we want max tempfile
# wx_data = soup.findAll(attrs = {'class':'wx-data'})

# print wx_data[5]

# dayTemp = wx_data[5].span.string
# print dayTemp old cold

import requests
import urllib2
from bs4 import BeautifulSoup

year = 2004
#create comma-delim file
f = open(str(year) + '_LAXwunder_data.txt','w')
#change the year here, ->run


#iterate through month and day
for m in range(1,13):
	for d in range(1,32): #could step 5 days using range(1,32,2)
		
		#Chk if already gone through month
		if (m == 2 and d > 28):
			break
		elif (m in [4,6,9,11]) and d > 30:
			break
		
		# open wug url
		timestamp = str(year)+'.'+str(m)+'.'+str(d)
		print 'Getting data for ' + timestamp
		url = 'http://www.wunderground.com/history/airport/LAX/'+str(year) + '/' + str(m) + '/' + str(d) + '/DailyHistory.html'
		page = urllib2.urlopen(url)
		#Get temp from page
		soup = BeautifulSoup(page)
		#dayTemp = soup.body.wx-data.b.string
		dayTemp = soup.findAll(attrs = {'class':'wx-data'})[5].span.string
	        humidity = soup.find(text='Average Humidity')
                next_cell = humidity.find_parent('td').find_next_sibling('td')
                avg_humidity = next_cell.string

		#Format month for timestamp
		if len(str(m)) < 2:
			mStamp = '0' + str(m)
		else:
			mStamp = str(m)
		#Format day for timestamp
		if len(str(d)) < 2:
			dStamp = '0' + str(d)
		else:
			dStamp = str(d)

		#Build timestamp
		timestamp = str(year)+ mStamp + dStamp

		#Wrtie timestamp and temp to file
		f.write(timestamp + ',' + dayTemp + ',' + avg_humidity + '\n')
		print dayTemp, avg_humidity

#done - close
f.close()


