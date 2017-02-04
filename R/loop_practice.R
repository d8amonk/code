x <- list()
for(i in 1:20){
  if(i%5 == 0)
    x <- append(x, i)
    t <- sum(x)
    print(t)
  else
    x <- append(x, i)}
