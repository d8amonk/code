3lottery <- function(a,b,c,d,e,f,g,h){
Ua <- function(x){sqrt(x)}
Ub <- function(x){log(x+1)}

pay_a <- function(x){x^2}
pay_b <- function(x){exp(x)-1}

L1 <- matrix(c(a,c))
P1 <- matrix(c(b,d))

L2 <- matrix(c((2*a),c,(a/2)))
P2 <- matrix(c(2*b),d,(b/2)))

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
colnames(df) <- c("$A to_play L1","$A to play L2","$B to play L1","$B to play L2")
df
