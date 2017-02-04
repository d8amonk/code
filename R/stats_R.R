bmi  <- rnorm(500, 23,2)
summary(bmi)
t.test(bmi, mu = 22.5)
args(plot.default)
pain  <- c(0,1,3,3,2,1,0,2,2,1,3,1)
length(pain)
pain  <- subset(pain, select = pain[1:10])
pain[1:10]
pain  <- pain[1:10]
length(pain)
pain
fpain  <- factor(pain, levels = 0:3)
levels(fpain)  <- c("none","low","med","high")
fpain
as.numeric(fpain)
levels(fpain)
summary(fpain)
plot(fpain)
plot(density(fpain))
