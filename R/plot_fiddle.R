x <- seq(-4,4,length = 200)
y <- dnorm(x, mean = 0, sd = 1)

plot(x,y, type = "l", lwd = 2)

plot(x,y, type = "l", lwd = 2, xlim = c(-3.5,3.5))

plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# alt
curve(dnorm, -3.5, 3.5, lwd=2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))
