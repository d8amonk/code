# temp is already defined in the workspace

temp <- list(
  c(3, 7, 9, 6, -1),
  c(6, 9, 12, 13, 5),
  c(4, 8 ,3, -1, -3),
  c(1, 4, 7, 2, -2),
  c(5, 7 ,9, 4, 2),
  c(-3, 5, 8, 9, 4),
  c(3, 6, 9, 4, 1)
)

# Convert to vapply() expression
vapply(temp, max, numeric(1))

# Convert to vapply() expression
vapply(temp, function(x, y) { mean(x) > y }, logical(1), y = 5)

# Definition of get_info (don't change)
get_info = function(x, y) { 
  if(mean(x) > y) {
    return("Not too cold!")
  } else {
    return("Pretty cold!")
  }
}

# Convert to vapply() expression
vapply(temp, get_info, character(1), y = 5)
