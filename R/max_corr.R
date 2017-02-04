set.seed(1984)
A <- rnorm(100,5,1)
B <- rnorm(100,6,1)
C <- rnorm(100,7,4)
D <- rnorm(100,4,2)
E <- rnorm(100,4,3)


M <- data.frame(A,B,C,D,E)
N <- cor(M)

N

(n <- max.col(`diag<-`(N,0)))
colnames(N)[n]
N[cbind(seq_len(nrow(N)),n)]
