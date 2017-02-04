complete <- function(directory, id){
  
  final.df <- data.frame()
  
  for(ii in id){
    file.name <- paste("C:/Data/", directory, "/", sep="")
    DF <- getmonitor(ii, file.name)[, c("sulfate", "nitrate")]
    sum.complete <- apply(DF, 1, sum)
    counter.complete <- sum(!is.na(sum.complete))
    out <- data.frame(id=ii, nobs=counter.complete)
    final.df <- rbind(final.df, out)
  }
  final.df
}  

x <-data.frame(a=rep(NA,332), b=rep(NA, 332))

x[i,] <- c(i, sum(complete.cases(getmonitor(i)))