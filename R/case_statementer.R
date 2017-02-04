p <- function(x){
    out <- list()
    for(i in x){
      out[i] <- paste("(case when placement = ", x[i], " then 1 else 0 end) as ", x[i], sep = '')
      return(out)
    }
}

big5 <- read.csv("c:/Users/jvangeete/Desktop/big5_conv_placements.csv")
p <- function(x){
  paste("(case when placement = \'", x, "\' then 1 else 0 end) as ", x, ",", sep = '')
}

big5 <- read.csv("c:/Users/jvangeete/Desktop/big5_conv_placements.csv")
out <- mapply(p, big5)
write.csv(out, "c:/Users/jvangeete/Desktop/case.csv", row.names = FALSE)
