###########
## try ####
###########
require(sparklyr)
require(rJava)

# devtools::install_github("rstudio/sparklyr")
# devtools::install_github("eddelbuettel/digest")
# Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_111\\jre')


##############
# deprecated #
##############
# if (!require('devtools')) install.packages('devtools')
# devtools::install_github('apache/spark@v1.4.0', subdir='R/pkg')
# jsonlite::fromJSON("https://api.github.com/repos/apache/spark/tags")$name
# library(SparkR) 

# options("sparkapi.ports.file" = "ports.out")

sc <- spark_connect(master = "local")#, config = list())

library(dplyr)
iris_tbl <- copy_to(sc, iris, "iris", overwrite = T)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights", overwrite = T)
batting_tbl <- copy_to(sc, Lahman::Batting, "batting", overwrite = T)
car_tbl <- copy_to(sc, mtcars, "cars", overwrite = T)
src_tbls(sc)

flights_tbl %>% filter(dep_delay > 2)

delay <- flights_tbl %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect()

# plot delays
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

# can we combine the dplyr construct of delay
# with the ggplot of its dimensions of interest?????

#consider these window functions.  each t in T needs an aggregator; split across RDDs?
library(Lahman)
batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H) 
batting <- arrange(batting, playerID, yearID, teamID)
players <- group_by(batting, playerID)

# For each player, find the two years with most hits
filter(players, min_rank(desc(H)) <= 2 & H > 0)
# Within each player, rank each year by the number of games played
mutate(players, G_rank = min_rank(G))

# For each player, find every year that was better than the previous year
filter(players, G > lag(G))
# For each player, compute avg change in games played per year
mutate(players, G_change = (G - lag(G)) / (yearID - lag(yearID)))

# For each player, find all where they played more games than average
filter(players, G > mean(G))
# For each, player compute a z score based on number of games played
mutate(players, G_z = (G - mean(G)) / sd(G))

# back to the sparkls
batting_tbl %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID, yearID, teamID) %>%
  group_by(playerID) %>%
  filter(min_rank(desc(H)) <= 2 & H > 0)
# check to see how to get this in memory

# It's also possible to execute SQL queries directly against 
# tables within a Spark cluster. The spark_connection object 
# implements a DBI interface for Spark, so you can use dbGetQuery 
# to execute SQL and return the result as an R data frame:
  
library(DBI)
iris_preview <- dbGetQuery(sc, "SELECT * FROM iris LIMIT 10")
iris_preview

# copy mtcars into spark
mtcars_tbl <- copy_to(sc, mtcars)

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 1099)

# fit a linear model to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))
summary(fit)
# is this any different than an ols regression? if not why do i care about spark?
summary(lm(mpg ~ wt + cyl, data = subset(mtcars, hp > 100)))
# yes ... whoa ...


@alikazaidi
origins <- file.path("wasb://mrs-spark@alizaidi.blob.core.windows.net",
                     "user/RevoShare/alizaidi/Freddie/Acquisition")
freddie_origins <- spark_read_csv(sc,
                                  path = origins,
                                  name = 'freddie_origins',
                                  header = FALSE,
                                  delimiter = "|"
)

# make sure to install
require("h2o")
install.packages("rsparkling")



d <- filter(mtcars, hp > 100)
glimpse(mtcars_tbl)

























