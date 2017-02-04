import requests as req
import json
import pandas as pd
# import numpy as np
# import pprint as pp
import os

url = 'http://api.parkwhiz.com/search/?lat=39.742043&lng=-104.991531&start=1476119660&end=1476130460&key=d7f78323035560d02287cb40977b035d'
response = req.get(url)
print(response.text)

# [method for method in dir(response) if callable(getattr(response, method))]
json_data = json.loads(response.text) # could just call on req.get(url).text

# Now you can access the data stored in datapoints just as you were expecting:
datapoints = json_data['parking_listings']
print(datapoints)

responsedf = pd.DataFrame(datapoints)
responsedf.to_csv(os.path.join(os.getcwd(), 'test_csv.csv', ))

# pp.pprint(responseJSON) #print pretty
# from urllib2 import urlopen
#
# kittens = urlopen('http://placekitten.com/200/300')
#
# f = open('kittens.jpg', 'wb')
# f.write(kittens.read())
# f.close()