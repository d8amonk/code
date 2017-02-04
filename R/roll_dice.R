roll  <- function(count = 1:6) {sample(count, size = 2, replace = T)}
roll() #[1] 6 1 #[2] 3 6
roll(1:10) #[1] 7 9
roll(1:2) #[1] 1 2 #[2] 1 1
