## ---- USStateData ----
require(datasets)

states <- data.frame(state.name, state.x77)

GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                           options=list(region="US", 
                                        displayMode="regions", 
                                        resolution="provinces",
                                        width=600, height=400))

plot(GeoStates)