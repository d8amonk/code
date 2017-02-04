x_1 <- rnorm(1000,5,1)
x_2 <- rnorm(1000,20,3)
x_3 <- x_2*x_1

hist(x_1, col = 'grey')

true_error1 <- rnorm(1000,0,3)
true_b0 <- 1.1
true_b1 <- 8.2
true_b2 <- -3
true_b3 <- 37


y_1 <- true_b0 + true_b1*x_1 + true_b2*x_2 + true_b3*x_3 + true_error1
hist(y_1)


plot(x_1, y_1, pch = 20, col = 'red')

run1 <- lm(y_1 ~ x_1 + x_2 + x_3)
run1

x_4 <- rgamma(1000,5,1)
x_5 <- rgamma(1000,20,3)
x_6 <- x_4^2

true_error2 <- rnorm(1000,0,3)
true_b_0 <- 11
true_b_1 <- 5.3
true_b_2 <- 2
true_b_3 <- -0.6


y_2 <- true_b_0 + true_b_1*x_4 + true_b_2*x_5 + true_b_3*x_6 + true_error2
hist(y_2)
  
plot(x_4, y_2, pch = 22, col = 'blue')

run2 <- lm(y_2 ~ x_4 + x_5 + x_6)
run2

vars <- list(x_1, x_2, x_3, x_4, x_5, x_6)
pairs(vars)
