# Assm Sales = Base + b1*Ad

sales <- c(37, 89, 82, 58, 110, 77, 103, 78, 95, 106, 98, 96, 68, 96, 157, 198, 145, 132, 96, 135)
ad <- c(6, 27, 0, 0, 20, 0, 20, 0, 0, 18, 9, 0, 0, 0, 13, 25, 0, 15, 0, 0)

# throw in adstock, assm now sales = Base + b1*f(Ad | adstock)
ad.adstock <- as.numeric(filter(x=ad, filter=.50, method="recursive"))

modFit.1 <- lm(sales~ad.adstock)
summary(modFit.1)