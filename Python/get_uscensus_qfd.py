states = {
        '01': 'Alaska',
        '02': 'Alabama',
        '03': 'Arkansas',
        '04': 'Arizona',
        '05': 'California',
        '06': 'Colorado',
        '07': 'Connecticut',
        '08': 'District of Columbia',
        '09': 'Delaware',
        '10': 'Florida',
        '11': 'Georgia',
        '12': 'Hawaii',
        '13': 'Iowa',
        '14': 'Idaho',
        '15': 'Illinois',
        '16': 'Indiana',
        '17': 'Kansas',
        '18': 'Kentucky',
        '19': 'Louisiana',
        '20': 'Massachusetts',
        '21': 'Maryland',
        '22': 'Maine',
        '23': 'Michigan',
        '24': 'Minnesota',
        '25': 'Missouri',
        '26': 'Mississippi',
        '27': 'Montana',
        '28': 'North Carolina',
        '29': 'North Dakota',
        '30': 'Nebraska',
        '31': 'New Hampshire',
        '32': 'New Jersey',
        '33': 'New Mexico',
        '34': 'Nevada',
        '35': 'New York',
        '36': 'Ohio',
        '37': 'Oklahoma',
        '38': 'Oregon',
        '39': 'Pennsylvania',
        '40': 'Rhode Island',
        '41': 'South Carolina',
        '42': 'South Dakota',
        '43': 'Tennessee',
        '44': 'Texas',
        '45': 'Utah',
        '46': 'Virginia',
        '47': 'Vermont',
        '48': 'Washington',
        '49': 'Wisconsin',
        '50': 'West Virginia',
        '51': 'Wyoming'
}

import urllib2
from bs4 import BeautifulSoup

#year = 2004
#create comma-delim file
#f = open(str(year) + 'uscen_qfd_v1.txt','w')
#change the year here, run

f = open('uscen_qfd_v1.txt','w')

#iterate through state and county
for s in range(1,51):
    for c in range(1,108): #could sample every 5th coutny using range(1,508,5)
        #loc = str(s)+'.'+str(c) #could I make this a dictionary (see below)***?
	
        if (c%2 == 0):
            break
        else:	    
            #Format county for loc_stamp
            if len(str(c)) == 1:
                c = '00' + str(c)
            elif len(str(c)) == 2:
                c = '0' + str(c)
            else:
                c = str(c)

        
            #Format state for loc_stamp
            if len(str(s)) == 1:
                s = '0' + str(s)
            else:
                s = str(s)
            #return s
        
        #Build loc_stamp and loc_eng
        loc_stamp = s + '/' + s + c 
        #loc_eng = #need the dictionary here *** UPDATE 1: found a dictionary, need to plug it in
        
        print 'Getting data for ' + loc_stamp
        url = 'http://quickfacts.census.gov/qfd/states/' + loc_stamp + '.html'
        page = urllib2.urlopen(url)
        soup = BeautifulSoup(page)
        
        #c_black_alone = soup.find_all("td", attrs={'headers':'rp9'})[0]
        #s_black_alone = soup.find_all("td", attrs={'headers':'rp9'})[1]
                
        c_t = soup.find_all("td", attrs={'headers':'rp9'})[0].text
        s_t = soup.find_all("td", attrs={'headers':'rp9'})[1].text
        c_f = float(str(c_t).split("%")[0])
        s_f = float(str(s_t).split("%")[0])
        c_share = round(c_f/100,3)
        s_share = round(s_f/100,3)
        
        state_list = soup.find_all("option")
        for state in state_list:
            print state['value'].split(".")[0], state.text
                        		        		        
        #humidity = soup.find(text='Average Humidity')
        #next_cell = humidity.find_parent('td').find_next_sibling('td')
        #avg_humidity = next_cell.string
        
        f.write(loc_stamp + ',' + str(c_share) + ',' + str(s_share) + '\n')
        print loc_stamp, c_share, s_share

#done - close
f.close()



