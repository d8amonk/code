combo_beast <- function(n, order, rep){
  switch(order,
         yes = switch(rep,
                      yes = {
                        y <- 0
                        for(i in 1:n){
                          x <- n^i
                          y <- x + y}
                        return(y)
                      },
                      no = {
                        y <- 0
                        for(i in 1:n){
                          x <- factorial(n + i - 1)/(factorial(i)*factorial(n-1))  
                          y <- x + y}
                        return(y)
                      }
                     ),
         no = switch(rep,
                     yes = {
                        y <- 0
                        for(i in 1:n){
                          x <- factorial(n)/(factorial(n - i))  
                          y <- x + y}
                        return(y)
                      },
                      no = {
                        y <- 0
                        for(i in 1:n){
                          x <- factorial(n)/(factorial(n-i)*factorial(i))
                          y <- x + y}
                        return(y)
                      }
                     )
        )
  }


combo_unicorn <- function(n, order, rep){
  if(order == 'yes' & rep == 'yes'){
    y <- 0
    for(i in 1:n){
      x <- n^i
      y <- x + y}
    return(y)
  } else if (order == 'yes' & rep == 'no'){
    y <- 0
    for(i in 1:n){
      x <- factorial(n + i - 1)/(factorial(i)*factorial(n-1))  
      y <- x + y}
    return(y)
  } else if (order == 'no' & rep == 'no'){
    y <- 0
    for(i in 1:n){
      x <- factorial(n)/(factorial(n-i)*factorial(i))
      y <- x + y}
    return(y)
  } else {
    y <- 0
    for(i in 1:n){
      x <- factorial(n)/(factorial(n - i))  
      y <- x + y}
    return(y)
  }
}

microbenchmark(combo_unicorn(5,'yes','yes'))
microbenchmark(combo_unicorn(5,'no','no'))
microbenchmark(combo_beast(5,'yes','yes'))
microbenchmark(combo_beast(5,'no','no'))

