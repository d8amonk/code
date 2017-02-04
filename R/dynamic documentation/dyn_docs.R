R.home('bin')



set.seed(1213) # for reproducibility of random numbers
x <- cumsum(rnorm(100))
plot(x, 
     type = "l",
     ylab = "$x_{i+1}=x_i + \\epsilon_{i+1}$",
     xlab = "step")



library(xtable)
xtable(head(mtcars[, 1:6]))



if (!require("shiny")) install.packages("shiny")
demo("notebook", 
     package = "knitr")
# Listening on http://127.0.0.1:3069



library(knitr)

knit_patterns$get()

knit_patterns$set(
  chunk.begin = "^<<r(.*)", 
  chunk.end = "^r>>$",
  inline.code = "\\{\\{([^}]+)\\}\\}"
)

knit_patterns$get(c("chunk.begin", 
                    "chunk.end",
                    "inline.code"))
