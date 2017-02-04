getmonitor <- function(id, directory, summarize = FALSE) {

  id <- if (as.numeric(id) < 10) {
    id <- paste("00",id,sep="")
    
  } else if(as.numeric(id) < 100) {
    id <- paste("0",id, sep="")
  } else if (as.numeric(id) >= 100) {
    id <- paste(id, sep="") 
  }
  filename <- paste(directory, id,".csv", sep="")
  data <- read.csv(filename)
  summarize <- if(summarize){ 
    print(summary(data))
  }
  return(data)
  
}  