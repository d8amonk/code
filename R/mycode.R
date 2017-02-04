myfunction <- function(){
  x <- rnorm(100)
  mean(x)
  sd(x)
}

second <- function(x){
  x + rnorm(length(x))
}
good <- complete.cases(hw1)
mean(hw1[good,]$Ozone)

help(subset)

#contour plot
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

#persp
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)



##random walk w/ a coinflip
while(z>=3 && z <= 10){
  print(z)
  coin <- rbinom(1,1,0.5)
  if(coin==1) {
  z <- z+1
  } else {
  z <- z-1
  }
}

#optimization algorithm
x0 <- 1
tol <- 1e-8
repeat{
  x1 <-computeEstimate()
  if (abs(x1-x0) < tol) {
    break
  } else {
      xo <-x1
  }
}

printmessage <- function(x) {
  if(x>0)
    print("x is greater than zero")
  else
    print("x is less than zero")
  invisible(x)
}