alpha_numeric <-  function(n = 100){
  x <- matrix(nrow = n, ncol = 3)
  colnames(x) <- c("letters", "numbers", "combo")  
#     data.frame(
#     letter = matrix(rep(NA, n)), 
#     number = matrix(rep(NA, n))
#     )
  for(i in 1:n){
    x[i,1] <- sample(letters, 1, replace = TRUE)
    x[i,2] <- sample(1:n, 1, replace = FALSE)
    x[i,3] <- paste(x[i,1], x[i,2], sep = '')
    
    # n <- sample(1:100, 1, replace = FALSE)
    # x[] <- paste(a, n, sep = ",")
  }
  return(data.frame(x))
}
