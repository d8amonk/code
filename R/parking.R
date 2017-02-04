require(XML)
require(curl)
require(RCurl)
require(jsonlite)
require(httr)

url = 'http://api.parkwhiz.com/search/?lat=39.74&lng=-104.99&start=1476119660&end=1476130460&key=d7f78323035560d02287cb40977b035d'
j = GET(url)
raw = content(j, as = 'text')

json = fromJSON(url, flatten = TRUE)
write(json, "test_json.json")
