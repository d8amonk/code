druid.lift <- function(datain, dataout) {
  library(stats)
  
  a <- datain[,2]
  b <- datain[,4]
  c <- datain[,3]
  d <- datain[,5]
  PID <- datain[,1]
  
  FElift <- numeric(0)
  FEpvalue <- numeric(0)
  i=numeric(0)
  for (i in 1:length(a)) {
    FElift<- c(FElift, fisher.test(matrix(c(a[i],b[i],c[i],d[i]),nrow=2))$estimate)
    FEpvalue <- c(FEpvalue, fisher.test(matrix(c(a[i],b[i],c[i],d[i]),nrow=2))$p.value)
    }
  
  FElift <- data.frame(FElift)
  FEpvalue <- data.frame(FEpvalue)
  liftmatrix <- cbind(PID,FElift,FEpvalue)
  liftmatrix$PID <- as.character(liftmatrix$PID)
  
  write.table(liftmatrix,file=dataout,sep="\t",row.names=FALSE)
  return(liftmatrix)
}
