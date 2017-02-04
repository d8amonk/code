auto <-Auto
View(auto)
dim(Auto)
Auto[1:4]
auto<-na.omit(auto)
dim(auto)
names(auto)


auto$Status
plot(auto$cylinders, auto$mpg)
attach(auto)
plot(cylinders, mpg)
pairs(~ mpg + displacement + horsepower + weight + acceleration , auto)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ", ylab ="MPG ")
hist(mpg ,col =2, breaks =15)

plot(horsepower ,mpg)
identify (horsepower ,mpg ,name)
plot(horsepower ,mpg)
identify (horsepower ,mpg ,name)
summary (Auto)

cylinders =as.factor (cylinders )
plot(cylinders , mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ", ylab ="MPG ")