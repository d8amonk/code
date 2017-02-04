setwd("C:/Users/Jeffrey/Google Drive/Code")

NPMRP_Data_2009_2010 <- read.csv("C:/Users/Jeffrey/Google Drive/Code/NPMRP_Data_2009_2010.csv", header=FALSE)

View(NPMRP_Data_2009_2010)

police <- NPMRP_Data_2009_2010
colnames(police)=c('loc','year','type','status','detail','url')

stat_lev <- levels(police$status)
length(stat_lev)
summary(stat_lev)

## ---- USStateData ----
require(datasets)

states <- data.frame(state.name, state.x77)

GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))

plot(GeoStates)