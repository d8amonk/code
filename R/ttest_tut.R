sigdiff <- function(n.x = 100, n.y = 100, mean.x = 10, mean.y = 8, sd.x = 7, sd.y = 5){
  require(ggplot2)
  require(reshape)
  x <- rnorm(n = n.x, mean = mean.x, sd = sd.x)
  y <- rnorm(n = n.y, mean = mean.y, sd = sd.y)
  
  par(mfrow = c(2,1))
  plot(density(x), main = paste("X ~ N(mean =", round(mean(x)), 
                                ", sd =", round(sd(x)),")"))
  abline(v = mean(x), col = 'red', lwd = 3)
  abline(v = c(mean(x) - sd(x), 
               mean(x) - 2*sd(x), 
               mean(x) - 3*sd(x),
               mean(x) + sd(x),  
               mean(x) + 2*sd(x), 
               mean(x) + 3*sd(x)),
         col = 'blue', 
         lwd = 2)
  
  plot(density(y), main = paste("Y ~ N(mean =", round(mean(y)), 
                                ", sd =", round(sd(y)),")"))
  abline(v = mean(y), col = 'red', lwd = 3)
  abline(v = c(mean(y) - sd(y), 
               mean(y) - 2*sd(y), 
               mean(y) - 3*sd(y),
               mean(y) + sd(y), 
               mean(y) + 2*sd(y), 
               mean(y) + 3*sd(y)),
         col = 'blue', 
         lwd = 2)
  
  t <- t.test(x,y) #Ho: mean(x) == mean(y)
  
  ifelse(t$p.value < 0.05,
         print("They're (Significantly) Different!"),
         print("They're NOT (Significantly) Different!"))
  
  df <- data.frame(x = x, y = y)
  df.m <- melt(df)
  ggplot(df.m) + 
  geom_density(aes(x = value,
                   y = ..density..,
                   color = variable, 
                   fill = variable)) + 
    ggtitle("Densities from a kernel density estimator")
}