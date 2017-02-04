# 1000,.1,0,.9,100,.25,0,.75
# HW2

lottery <- function(a,b,c,d,e,f,g,h) {

Ua <- function(x){sqrt(x)}
Ub <- function(x){log(x+1)}

pay_a <- function(x){x^2}
pay_b <- function(x){exp(x)-1}

L1 <- matrix(c(a,c))
P1 <- matrix(c(b,d))
L2 <- matrix(c(e,g))
P2 <- matrix(c(f,h))

ev1 = sum(L1*P1)
ev2 = sum(L2*P2)

u <- data.frame(L1 = L1, P1 = P1, E1 = L1*P1, L2 = L2, P2 = P2, E2 = L2*P2)
u <-tbl_df(u)
Ua1<-u$U_a1 <- Ua(u$L1)*P1
Ub1<-u$U_b1 <- Ub(u$L1)*P1
Ua2<-u$U_a2 <- Ua(u$L2)*P2
Ub2<-u$U_b2 <- Ub(u$L2)*P2

u$Pay_a1 <- pay_a(Ua1)
u$Pay_b1 <- pay_b(Ub1)
u$Pay_a2 <- pay_a(Ua2)
u$Pay_b2 <- pay_b(Ub2)

A <- function(x,y) {
  if(x>y) 
    print(paste("Pl.A Prefers Lottery 1 bc L1(",sum(Ua1),") > L2(",sum(Ua2),")"))
  else 
    print(paste("Pl.A prefers Lottery 2 bc L1(",sum(Ua1),") < L2(",sum(Ua2),")"))
}
B <- function(x,y) {
  if(x>y) 
    print(paste("Pl.B Prefers Lottery 1 bc L1(",sum(Ub1),") > L2(",sum(Ub2),")"))
  else 
    print(paste("Pl.B prefers Lottery 2 bc L1(",sum(Ub1),") < L2(",sum(Ub2),")"))
}
A(sum(Ua1),sum(Ua2))
B(sum(Ub1),sum(Ub2))

df<-data.frame(sum(u$Pay_a1), 
           sum(u$Pay_b1), 
           sum(u$Pay_a2),
           sum(u$Pay_b2))
colnames(df) <- c("$A to not play L1","$A to not play L2","$B to not play L1","$B to not play L2")
df
}

require(pracma)
require(dplyr)

v1<-matrix(c(0,1,5))

p1 <- matrix(c(0,1,0))
p2 <- matrix(c(.01,.89,.1))
p3 <- matrix(c(.89,.11,0))
p4 <- matrix(c(.9,0,.1))
a<-v1*p1
b<-v1*p2
c<-v1*p3
d<-v1*p4
x<-(data.frame(a,b,c,d))

s<-summarise_each(tbl_df(x),funs(sum))
rank(s)

# HW3
t <- 1
M <- matrix(c(0,4,2,1),ncol=2)
rref(M)
p = 1/3
q = 1/2

p1 = 1
q1 = 1

p0 = 0
q0 = 0

p1<-matrix(c(p1,1-p1))
q1<-matrix(c(q1,1-q1))
p0<-matrix(c(p0,1-p0))
q0<-matrix(c(q0,1-q0))

allp1 <- M %*% p1
allp0 <- M %*% p0

allq1 <- M %*% q1
allq0 <- M %*% q0

p1<-c(0,allp0[1])
p2<-c(0,allp0[2])
p3<-c(1,allp1[1])
p4<-c(1,allp1[2])
p1
p2
p3
p4

A <- cbind(p3 - p1, p4 - p2)
b <- (p4 - p2)
a <- solve(A, b)
points((p1 + a[1]*(p2-p1))[1], (p1 + a[1]*(p2-p1))[2], pch = 19, col = "blue")
# NOT DONE YET!

A <- matrix(c(1, 2, 3, 1, 3, 2, 3, 2, 1), 3, 3, byrow = TRUE)
A
rref(A)
# [,1] [,2] [,3]
# [1,] 1 0 0
# [2,] 0 1 0
# [3,] 0 0 1
A <- matrix(data=c(1, 2, 3, 2, 5, 9, 5, 7, 8,20, 100, 200),
            nrow=3, ncol=4, byrow=FALSE)
A
rref(A)
# 1 0 0 120
# 0 1 0 0
# 0 0 1 -20
# Use rref on a rank-deficient magic square:
A = magic(4)
A
R = rref(A)
zapsmall(R)
# 1 0 0 1
# 0 1 0 3
# 0 0 1 -3
# 0 0 0 0

plot(c(0, 5), c(0, 5), type="n",
     xlab = "", ylab = "", main = "Segment Intersection")
grid()
## Not working yet:


pickup_sticks <- function(n){
for (i in 1:n) {
  s1 <- matrix(runif(4), 2, 2)
  s2 <- matrix(runif(4), 2, 2)
  if (segm_intersect(s1, s2)) {
    clr <- "red"
    p1 <- s1[1, ]; p2 <- s1[2, ]; p3 <- s2[1, ]; p4 <- s2[2, ]
    A <- cbind(p2 - p1, p4 - p3)
    b <- (p3 - p1)
    a <- solve(A, b)
    points((p1 + a[1]*(p2-p1))[1], (p1 + a[1]*(p2-p1))[2], pch = 19, col = "blue")
  } else
    clr <- "darkred"
  lines(s1[,1], s1[, 2], col = clr)
  lines(s2[,1], s2[, 2], col = clr)
}
}
## End(Not run)

# ROW ELIMINATION
A <- data.frame(matrix(c(5,4,0,1,4,3,-1,-2,1,2,4,1,0,-1,3,2), ncol=4))
A
#Columns
if(mean(A$X1>A$X2)==1) A<-A %>% select(-X1)
A
#Rows
if(mean(A[4,]<A[3,])==1) A<-A[-4,]
A
#Columns

if(mean(A$X3>A$X4)==1) A<-A %>% select(-X3)
A
#Columns

if(mean(A[2,]<A[1,])==1) A<-A[-2,]
A

A <- subset(A, select = -A[,2])
