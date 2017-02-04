par(bg=rgb(245,245,245, maxColorValue = 255))
plot(density(rnorm(1000000,mean = 658,sd = 350)), 
     xlim = c(400,1000),
     yaxt = "n",
     cex.main = 1.5,
     lwd = 3,
     ylab ="",
     xlab = "Expenditure in $Millions",
     main = "Testing The Estimates")
abline(v = 658, col = 'blue', lwd = 2)
abline(v = 742, col = 'red', lwd = 1, lty =3)
abline(v = 800, col = 'red', lwd = 1.5, lty =2)
abline(v = 930, col = 'red', lwd = 2)

text(x = 600, y = 0.0005, "$658M", col = "blue", font = 4, cex = 1)
text(x = 768, y = 0.00045, "$742M", col = "red", font = 2, cex = .7)
text(x = 835, y = 0.00055, "$800M", col = "red", font = 2, cex = .8)
text(x = 955, y = 0.0005, "$930M", col = "red", font = 4, cex = 1)

text(x = 600, y = 0.00045, "50th Percentile", col = "blue", font = 2, cex = .5)
text(x = 770, y = 0.00042, "56th Pctl.", col = "red", font = 2, cex = .5)
text(x = 837, y = 0.00052, "66th Pctl.", col = "red", font = 2, cex = .6)
text(x = 960, y = 0.00046, "80th Pctl.", col = "red", font = 2, cex = .7)
text(x = 965, y = 0.00043, "(Less than +1 SD)", col = "red", font = 4, cex = .5)

text(x = 600, y = 0.000425, "(Median)", col = "blue", font = 2, cex = .5)
