plot(jitter(Carbon) ~ jitter(City),
     xlab = "City MPG", 
     ylab = "Carbon footprint (tonnes per year)")

fit <- lm(Carbon ~ City, fuel)
abline(fit)

summary(fit)
