require(dplyr)
my_db <- src_sqlite("my_db.sqlite3", create = T)
class(my_db)

require(nycflights13)
flights_sqlite <- copy_to(my_db, flights, temporary = FALSE, 
                           indexes = list(
                             c('year', 'month', 'day'), 'carrier', 'tailnum'
                           ))

flights_sqlite <- tbl(nycflights13_sqlite(), "flights")
flights_sqlite

tbl(my_db, sql("SELECT count(distinct(month)) FROM flights"))

select(flights_sqlite, year:day, dep_delay, arr_delay)
filter(flights_sqlite, dep_delay > 240)
arrange(flights_sqlite, year, month, day)

mutate(flights_sqlite, speed = air_time / distance)
# note: not changed in place:
flights_sqlite

summarise(flights_sqlite, delay = mean(dep_time))

# dplyr tries to be as lazy as possible:
# It never pulls data into R unless you explicitly ask for it.
# It delays doing any work until the last possible moment:
# It collects together everything you want to do and then sends it to the database in one step.

# This sequence of operations never actually touches the database. 
# It's not until you ask for the data (e.g. by printing c4) that dplyr 
# generates the SQL and requests the results from the database. Even then it only pulls down 10 rows.

c1 <- filter(flights_sqlite, year == 2013, month == 1, day == 1)
c2 <- select(c1, year, month, day, carrier, dep_delay, air_time, distance)
c3 <- mutate(c2, speed = distance / air_time * 60)
c4 <- arrange(c3, year, month, day, carrier)
c4

# To pull down all the results use collect(), which returns a tbl_df():
collect(c4)
c4$query # NULL
explain(c4) # NULL



