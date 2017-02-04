library(dplyr)
library(ggplot2)

# load diamond data from ggplot library
data(diamonds)
# it's only a "promise" object until you...
# inspect the dataset
glimpse(diamonds)
# then it gets loaded into memory

# histogram = binning stat + bar geom + fill aes
p <- ggplot(diamonds, aes(x = carat)) #mapping
p # can't plot! no geom layer!
p + layer(
  geom = "bar", #geom
  geom_params = list(fill = "steelblue"), #aes
  stat = "bin", #stat
  stat_params = list(binwidth = 0.1) #stat
  )

p + geom_density() # fiddle with color

a <- ggplot(diamonds, aes(x = carat, y = price))
b <- ggplot(diamonds, aes(x = carat, y = price, 
                          color = color, 
                          size = carat, 
                          shape = cut))
a # no geom layer! can't plot!
b # no geom layer! can't plot!
a + geom_point(aes(color = color, size = size))
a + geom_point() + facet_wrap( ~ clarity)
a + geom_point() + facet_wrap(color  ~ clarity)
b + geom_point()
b + geom_point() + facet_wrap( ~ clarity)
 

# you can add multiple geom layers
ggplot(diamonds, aes(carat, price)) + geom_point() + geom_smooth()
# and change the smoothing method - here, linear
ggplot(diamonds, aes(carat, price)) + geom_point() + geom_smooth(method="lm")

ggplot(diamonds, aes(x=carat, y=price, col=cut)) + 
  geom_point() + 
  facet_grid(clarity ~ color)
