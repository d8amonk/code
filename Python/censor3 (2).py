f = open('places.csv')
text = str(f.read())
f.close()
places = text.replace(', USA','')

#It's then very simple to recreate your dataframe using string operations:

t1 = places.split('\n')
t2 = [p.replace(' ','').strip() for p in t1]
final_places = [p.split(',') for p in t2]

#To get cities/states:

cities = [p[0] for p in final_places]
states = [p[1] for p in final_places]