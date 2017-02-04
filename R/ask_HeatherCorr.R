x <- read.csv("c:/Users/jvangeete/Desktop/Asks/Heather/TDF_corr.csv", header = TRUE)
x[is.na(x)] <- 0
y <- cor(x)
y[is.na(y)] <- 0

diag(y) <- 0

# try 1
(n <- max.col(`diag<-`(y,0)))
colnames(y)[n]
y[cbind(seq_len(nrow(y)),n)]

y[n]

# try 2
colnames(N)[apply(N, 1, function (x) which(x==max(x[x<1])))]

# try 3
library(data.table)
setDT(melt(y))[Var1 != Var2, .SD[which.max(value)], keyby=Var1]
