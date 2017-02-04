# newtons sqrt method
sqrnt=function(y) {
  +     x=y/2
  +     while (abs(x*x-y) > 1e-10) x=(x+y/x)/2
  +     x
  + }

bool = T; i = 0
while (bool == T) {i = i + 1; bool=(i<10)} # stop at i = 11

s=0;x=rnorm(10000)

system.time(for (i in 1:length(x)){ # output sum(x) and
  s=s+x[i]})[3]