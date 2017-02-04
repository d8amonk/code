# The tic-tac-toe matrix has already been defined for you

# define the double for loop
for(i in 1:nrow(ttt)) {
  for(j in 1:ncol(ttt)) {
    print(paste("On row ",i," and column ", j, "the board contains",ttt[i,j]))
  }
}


# The linkedin vector has already been defined for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)

# Adapt/extend the for loop
for(li in linkedin) {
  if(li > 10) {
    print("You're popular!")
  } else {
    print("Be more visible!")
  }
  
  # Add code to conditionally break iteration
  if(li > 16) {
    print("This is ridiculous, I'm outta here!")
    break
  }
  
  # Add code to conditionally skip iteration
  if(li < 5) {
    print("This is too embarassing...")
    next
  }
  
  print(li)
}

# Pre-defined variables
rquote <- "R's internals are irrefutably intriguing"
chars <- strsplit(rquote, split = "")[[1]]

# Your solution here
rcount <- 0
for(char in chars) {
  if(char == "u" | char == "U") {
    break
  }
  if(char == "r" | char == "R") {
    rcount <- rcount + 1
  }
}

# Print the resulting rcount variable to the console
rcount