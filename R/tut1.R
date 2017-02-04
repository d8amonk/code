mat5<-matrix(rnorm(20,4)
#(20/nrow4)=5 columns!!

#appending v1 to mat5 BY COLUMN
v1 <- c(1, 1, 2, 2)
mat6 <- cbind(mat5, v1)

#appending v2 to mat6 (mat5 won't work - 
#too few columns now bc v2(dim)=1x6 and mat5(dim)=4x5
#but mat6(dim) with cbind is now 4x6)
mat7<-rbind(mat6,v2)
dim(mat7)
#should be 5x6 now
dimnames(mat7)<-list(NULL,NULL)
mat[1,6]
#firstcolumn, secondow =1
mat[1,]
mat[,6]
#show a row/col: leave out col/row
mat8<-matrix(1:6,2)
mat9<-matrix(c(rep(1,3),rep(2,3)),2, byrow=T)
#add
mat8+mat9
mat9+3
#subtr
mat8-mat9

solve(mat8[, 2:3]) #solve for the 2x2 submatrix of columns 2&3)
t(mat9) #transpose of matrix 9
mat8 %*% t(mat9) #matrix multiplication
mat8*mat9 #element multiplication - i.e. useless

hsb2 <- read.table("C:/datapile/hsb2.txt", header=T, quote="\"")
View(hsb2)
hsb2

x<-as.matrix(cbind(1,hsb2$math, hsb2$science, hsb2$socst,hsb2$female))
#column-binding a 200(=i)x 5 variable (1,math, sciecne, socst,female) mat

y[1:10,]
x[1:10,]

n<-nrow(x)
p<-ncol(x)
n
p

beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y
beta.hat #look at it

dimnames(beta.hat)<-list(c(1,"math","science","socst","female"),"values")

#predicted values
y.hat <- x %*% beta.hat
y.hat[1:5,1]
[1] 46.43465 60.75571 46.17103 49.51943 53.66160
y[1:5,1]
[1] 52 59 33 44 52

#the variance, residual standard error and df's
sigma2 <- sum((y - y.hat)^2)/(n - p)

#residual standard error
sqrt(sigma2)
[1] 6.101191

#degrees of freedom
n - p
[1] 195

#the standard errors, t-values and p-values for estimates
#variance/covariance matrix
v <- solve(t(x) %*% x) * sigma2

#standard errors of the parameter estimates
sqrt(diag(v))
[1] 2.81907949 0.06393076 0.05804522 0.04919499 0.88088532

#combine betahats and stderrs into a pretty table
results<-as.matrix(cbind(beta.hat[,1],sqrt(diag(v))))
dimnames(results)<-list(c(1,"math","science","socst","female"),c("betahat", "stderr"))
results

#t-values for the t-tests of the parameter estimates
t.values <- beta.hat/sqrt(diag(v))
t.values
#append the t-values to the pretty results table
results<-as.matrix(cbind(results,t.values))

#p-values for the t-tests of the parameter estimates
pval<-2 * (1 - pt(abs(t.values), n - p))
pval

#touch-up the table: 
dimnames(t.values)<-list(c(1,"math","science","socst","female"), "tvals")
dimnames(pval)<-list(c(1,"math","science","socst","female"), "pvals")
results<-as.matrix(cbind(results,t.values,pval))
