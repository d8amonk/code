x <- read.csv('c:/Users/jvangeete/Desktop/viewability_test.csv')
plot(x$VIEWABLE_IMPRESSIONS, x$TAPAD_ACT)
x$BIN <- as.factor(x$BIN)
x_lm <- lm(x$TAPAD_ACT/x$IMPRESSIONS_WON ~ x$VIEWABLE_IMPRESSIONS*x$BIN)
summary(x_lm)

abline(x_lm, col = 'red', lwd = 2)

library(ggplot2)
library(ggthemes)

ggplot(x, aes(x = IMPRESSIONS_WON, y = TAPAD_ACT, color = BIN)) + geom_point(size = 3) + 
  geom_smooth(alpha = 0.2, span = 0.5) + facet_grid(PLATFORM ~ BIN) + ggtitle("Activations By Impressions Won, By Bin By Desktop") +
  theme_fivethirtyeight() + scale_fill_fivethirtyeight()

ggplot(x, aes(IMPRESSIONS_WON, VIEWABILITY_RATE)) + geom_point() + geom_smooth()

