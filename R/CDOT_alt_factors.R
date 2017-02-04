x <- ts(matrix(rnorm(300,100,3)), start = c(1961,1), frequency = 12)



m <- matrix(data = as.numeric(x), ncol = 12, byrow = TRUE)
t <- t(m)
colnames(t) <- c(1961:1985)
rownames(t) <- month.name
