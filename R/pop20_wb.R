require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)
options(scipen = 999, digits = 3)

d <- read_csv("C://Users//jvangeete//Desktop//pop20csv.csv")

summary(d)
glimpse(d)
d <- d %>% gather(Year, Value, -c(1:4))
d$Value <- as.integer(d$Value)


x <- split(d, d$`Series Name`)
for (i in 1:length(x)){
  assign(paste('df', i, sep = '_'), x[i]))
}
df_1 <- as.data.frame(df_1)
colnames(df_1) <- colnames(d)
rename(df_1, Country = df_1[,3])

ggplot(df_1, aes(Year, Value, color = )) + geom_line()
