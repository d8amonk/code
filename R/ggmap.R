library(ggmap)

gc <- geocode("denver, colorado")
center <- as.numeric(gc)
ggmap(get_googlemap(center = center, scale = 1), extent = "device")
# ggmap(get_googlemap(center = center, color = "bw", scale = 2), extent = "device")

map <- get_googlemap(style = c(feature = "all", element = "labels", visibility = "off"))
ggmap(map)
