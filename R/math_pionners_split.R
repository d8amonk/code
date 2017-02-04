# Code from previous exercise:
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split_low <- lapply(split, tolower)

# Write function select_first()
select_first <- function(x){
  x[1]
}

# Apply select_first() over split_low: names
names <- lapply(split_low, select_first)

# Write function select_second()
select_second <- function(x){
  x[2]
}

# Apply select_second() over split_low: years
years <- lapply(split_low, select_second)


# C&C these two methods
# Argument is mentioned explicitly here. 
# This tells R to match the arguments by name!
multiply <- function(x, factor) {
  x*factor
}
lapply(list(1,2,3), multiply, factor = 3)
# This tells R to match the arguments by position!
lapply(list(1,2,3), multiply, 3)

# Replace the select_* functions above with a "Selection super-function" 
select_el <- function(x, index){
  return(x[index])
}

# Call the select_el() function twice on split_low: names and years
names <- lapply(split_low,select_el,1)
years <- lapply(split_low,select_el,2)


# Cities example
cities <- c("New York", "Paris", "London", "Tokyo", "Rio de Janeiro", "Cape Town")

unlist(lapply(cities, nchar))

sapply(cities, nchar)

# SUPER USEFUL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
first_and_last <- function(name){
  name <- gsub(" ","",name)
  letters <- strsplit(name, split = "")[[1]]
  c(first = min(letters), last = max(letters))
}

first_and_last("New York")

sapply(cities, first_and_last)

# so that when you make a comment it doesn't run the compiler


