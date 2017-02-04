## Crime Incident Plots
library(ggplot2)
library(foreign)
library(stringr)
library(lubridate)
library(plyr)
library(xtable)
library(scales)
library(RColorBrewer)
library(ggmap)

## gis libraries
library(maptools)
library(sp)
library(rgdal)
library(spatstat)

?readOGR

#get API key from http://www.census.gov/developers/tos/key_request.html

library(acs)
api.key.install('<778e43bd4e557e01e5d160dc14f6750ce2f408b3>')
