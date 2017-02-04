autoTransform <- function(x){
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}