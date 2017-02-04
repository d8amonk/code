star_string = "Proxima Centauri  M5  e      11.05 15.49 771.8"

star_name = star_string[0:17] 
spectral_class = star_string[18:28]   
apparent_magnitude = star_string[29:34]  
absolute_magnitude = star_string[35:40] 
parallax = star_string[41:46]   

print "Star name:         ", star_name
print "Spectral class:    ", spectral_class
print "Apparent magnitude:", apparent_magnitude
print "Absolute magnitude:", absolute_magnitude
print "Parallax:          ", parallax