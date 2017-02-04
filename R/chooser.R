chooser = function(n){
  m <- max(n)
  n <- m
  x <- 1:m
  #y <- 1:m
  for(i in 1:m)
    x[i] <- choose(m,i)
    par(mfrow = c(ceiling(m/2),floor(m/2)),
        bg=rgb(runif(1, 0, m),
               runif(1, 0, m),
               runif(1, 0, m),
               maxColorValue = m))
  for(i in n) { 
    plot(i,
         type = 'b', 
         pch = sample(1:25, 25, replace=T),
         lwd = 10,
         col = 1:m, 
         xlab = paste("k ~~> ",n), 
         ylab = "# of ways", 
         main = "n-choose-k curve")
    return(i)

  }
}