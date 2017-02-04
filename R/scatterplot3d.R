library(scatterplot3d)

# helix
z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "Helix", pch = 20)

# hemisphere
temp <- seq(-pi, 0, length = 50)
x <- c(rep(1, 50) %*% t(cos(temp)))
y <- c(cos(temp) %*% t(sin(temp)))
z <- c(sin(temp) %*% t(sin(temp)))
scatterplot3d(x, y, z, highlight.3d = TRUE, angle = 120,
              col.axis = "blue", col.grid = "lightblue", cex.axis = 1.3,
              cex.lab = 1.1, main = "Hemisphere", pch = 20)

# multivariate
data(trees)
s3d <- scatterplot3d(trees, type = "h", color = "blue",
                     angle = 55, scale.y = 0.7, pch = 16, main = "Adding elements")
my.lm <- lm(trees$Volume ~ trees$Girth + trees$Height)
s3d$plane3d(my.lm)
s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)

# Bivariate normal distribution
library("mvtnorm")
x1 <- x2 <- seq(-10, 10, length = 51)
dens <- matrix(dmvnorm(expand.grid(x1, x2),
                       sigma = rbind(c(3, 2), c(2, 3))),
               ncol = length(x1))
s3d <- scatterplot3d(x1, x2,
                     seq(min(dens), max(dens), length = length(x1)),
                     type = "n", grid = FALSE, angle = 70,
                     zlab = expression(f(x[1], x[2])),
                     xlab = expression(x[1]), ylab = expression(x[2]),
                     main = "Bivariate normal distribution")
text(s3d$xyz.convert(-1, 10, 0.07),
     labels = expression(f(x) == frac(1, sqrt((2 * pi)^n *
                                                phantom(".") * det(Sigma[X]))) * phantom(".") * exp * {
                                                  bgroup("(", - scriptstyle(frac(1, 2) * phantom(".")) *
                                                           (x - mu)^T * Sigma[X]^-1 * (x - mu), ")")}))
text(s3d$xyz.convert(1.5, 10, 0.05),
     labels = expression("with" * phantom("m") *
                           mu == bgroup("(", atop(0, 0), ")") * phantom(".") * "," *
                           phantom(0) *
                           {Sigma[X] == bgroup("(", atop(3 * phantom(0) * 2,
                                                         2 * phantom(0) * 3), ")")}))
for(i in length(x1):1)
  s3d$points3d(rep(x1[i], length(x2)), x2, dens[i,], type = "l")
for(i in length(x2):1)
  s3d$points3d(x1, rep(x2[i], length(x1)), dens[,i], type = "l")
