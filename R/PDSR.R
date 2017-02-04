
#5.5
d <- data.frame(y=(1:10)^2,
                x=1:10)
model <- lm(y~x, data = d)
d$yhat <- predict(model,data=d)

library(ggplot2)
ggplot(data=d) + 
geom_point(aes(x=x, y=y)) +
geom_line(aes(x=x,y=predict), color='blue') +
geom_segment(aes(x=x,y=yhat,yend=y,xend=x) +
scale_y_continuous('')
  
#RMSE = 7.27
sqrt(mean((d$yhat-d$y)^2))
#R^2=0.95
1-sum((d$yhat-d$y)^2)/sum((mean(d$y)-d$y)^2)
