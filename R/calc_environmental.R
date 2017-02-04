# require(HistData)
require(dplyr)
# require(resample)
require(ggplot2)
require(Deriv)
require(rootSolve)

# data(Galton)
# data(GaltonFamilies)
# 
# glimpse(Galton)
# summary(GaltonFamilies)
# 
# table(GaltonFamilies$family)[1][10]
# sample(GaltonFamilies[,1:3],, replace = T)

mac1 = function(x)(x^3-3*x^2+5)
mac2 = function(x)(x^4+4*x^2+4)
mac3 = function(x)(x^3 + 3 * x^2 - 6 * x - 8)

ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  stat_function(fun = mac1, color = 'green', alpha = 0.6, lwd = 2) +
  # ggplot(data.frame(x = c(0, 8)), aes(x = x)) +
  stat_function(fun = mac2, color = 'purple', alpha = 0.6,  lwd = 2) +
  # ggplot(data.frame(x = c(0, 8)), aes(x = x)) +
  stat_function(fun = mac3, color = 'blue', alpha = 0.6,  lwd = 2)

f1 <- body(mac1)[[2]]
f2 <- body(mac2)[[2]]
f3 <- body(mac3)[[2]]

g1 <- function(x){}
g2 <- function(x){}
g3 <- function(x){}

body(g1) <- D(f1, 'x')
body(g2) <- D(f2, 'x')
body(g3) <- D(f3, 'x')

par(mfrow = c(1,3))
curve(mac1, 0, 5, ylim = c(-5,10))
curve(g1, 0, 5, add = T, col = 'red', lwd = 2)
curve(mac2, -5, 5)
curve(g2, -5, 5, add = T, col = 'red', lwd = 2)
curve(mac3, 0, 5)
curve(g3, 0, 5, add = T, col = 'red', lwd = 2)

roots1 <- multiroot(g1, c(-5, 5))
roots2 <- multiroot(g2, c(-5, 5))
roots3 <- multiroot(g3, c(-5, 5))
mac1(roots1$root)
mac2(roots2$root)
mac3(roots3$root)

curve(mac1, -5, 5, ylab = "mac1 = f(x)")
points(mac1(roots1$root) ~ roots1$root, pch = 8, col = 'red', cex = 5)
curve(mac2, -5, 5, ylab = "mac2 = f(x)")
points(mac2(roots2$root) ~ roots2$root, pch = 8, col = 'red', cex = 5)
curve(mac3, -5, 5, ylab = "mac3 = f(x)")
points(mac3(roots3$root) ~ roots3$root, pch = 8, col = 'red', cex = 5)

#   stat_function(fun = dnorm, args = list(0.2, 0.1)) +
#   stat_function(fun = dnorm, args = list(0.7, 0.05))