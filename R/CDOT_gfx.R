# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////
# ///       ***THIS IS THE GRAPHICS SECTION***       ///
# ///  THIS SECTION RUNS ON EXP.DRAW, EXP.S OBJECTS  ///
# //////////////////////////////////////////////////////
# //////////////////////////////////////////////////////


######################################################
#####################       UPCOMING TASKS         ###
######################################################

######################################################
#####################        LOADING AREA          ###
######################################################

######################################################
#####################   BUILDING & TRAINING AREA   ###
######################################################

######################################################
#####################         FITTING AREA         ###
######################################################
lm(Amount ~ Date + len + num_payments + tot, exp.s)
fit <- lm(Amount ~ Date + len + num_payments + tot, exp.s)
plot(fit)

######################################################
#####################       PLOTTING AREA          ###
######################################################

#LOAD EXP.S FROM _FORECAST
half_sampl = exp.s[sample(nrow(exp.s), 1000),]
synthetic_sampl = exp.s[sample(nrow(exp.s), 10000, replace = T),]

###### pastebin

x <- ggplot(exp.s, aes(x = exp.s$Year, y = exp.s$Amount, z= exp.s$num_payments))
x2 <- x + geom_jitter(aes(color = as.factor(start_year), size = tot))
x3 <- x2 + geom_density()

x2
x3

#C/P FOR OPTIONS
dev.off()
par(mfrow = c(2,1))
grid.draw(panel)

theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())

#XPLANT FROM _FORECAST.R, WORK-IN?


plot(exp.c$c_tot,
     exp.c$len,
     pch = 1,
     xlab = "$$$", 
     ylab = "# Days", 
     main = "Does a contract's total budget drive its length in days?",
     sub = "THIS IS ONLY FOR DEMO PURPOSES")
#col=rgb(1,0,0,.8)), maxcolorValue=255?
abline(lm(len ~ c_tot, exp.c), col="red", lwd = 2, lty = 1)
plot(exp.c$c_tot, 
     exp.c$num_payments, 
     xlab = "$$$", 
     ylab = "# of Payments", 
     main = "What about the number of payments we have to make (i.e. does a larger $ contract have more or larger payments)?",
     sub = "THIS IS ONLY FOR DEMO PURPOSES")
abline(lm(num_payments ~ c_tot, exp.c), col="red", lwd = 2, lty = 1)
#reset graphics device
dev.off()


plot(density(exp.s$Amount))

p <- ggplot(exp.s, aes(x=Date, y=Amount, z=num_payments)) 
# p + stat_density(z)
pp <- p + geom_point(alpha = .5, size = 4)
pp
ps <- p + stat_smooth(method = "lm", formula = y ~ x, size = 3, se = FALSE,colour = "orange", alpha = 0.8) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),size = 5, se = TRUE, colour = "blue", alpha = 0.8) + 
  stat_smooth(method = "loess", formula = y ~x, size = 3, se = FALSE, colour = "red", alpha = 0.8) + 
  stat_smooth(method = "gam", formula = y ~s(x), size = 3, se = FALSE, colour = "green", alpha = 0.8) + 
  stat_smooth(method = "gam",formula = y ~ s(x, k = 3), size = 3, se = FALSE, colour = "purple")
pp + theme_solarized(light=T) #+ scale_color_grey()
ps + theme_solarized(light=T) #+ scale_color_grey()
pps1 <- pp + stat_smooth(method = "lm", formula = y ~ x, size = 2, se = FALSE,colour = "orange")
pps2 <- pp + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 3, se = TRUE, colour = "blue")
pps3 <- pp + stat_smooth(method = "loess", formula = y ~x, size = 2, se = FALSE, colour = "red")
pps4 <- pp + stat_smooth(method = "gam", formula = y ~ s(x), size = 2, se = FALSE, colour = "green")
pps5 <- pp + stat_smooth(method = "gam",formula = y ~ s(x, k = 5), size = 2, se = FALSE, colour = "purple")
pps1 #+ ylim(c(0,50000))
pps2 #+ ylim(c(0,50000))
pps3 #+ ylim(c(0,50000))
pps4 #+ ylim(c(0,50000))
pps5 #+ ylim(c(0,50000))
# grid arrange!

ppsa <- pp + stat_smooth(method = "lm", formula = y ~ x, size = 3, se = FALSE,colour = "orange", alpha = 0.8) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),size = 5, se = TRUE, colour = "blue", alpha = 0.8) + 
  stat_smooth(method = "loess", formula = y ~x, size = 3, se = FALSE, colour = "red", alpha = 0.8) + 
  stat_smooth(method = "gam", formula = y ~s(x), size = 3, se = FALSE, colour = "green", alpha = 0.8) + 
  stat_smooth(method = "gam",formula = y ~ s(x, k = 3), size = 3, se = FALSE, colour = "purple")
ppsa #+ ylim(c(0,200000))

q <- ggplot(exp.s, aes(x=Date, y=Amount), size = tot, alpha = I(.7), color = as.factor(start_year))
q + geom_point() + 
  theme(axis.ticks=element_blank(),
        axis.text.x=element_blank()) +
  scale_color_gdocs() + 
  guides(color = F, size = F)

q <- ggplot(exp.s, aes(x=Date, y=Amount), size = tot, alpha = I(.7), color = as.factor(start_year))
q + geom_jitter(aes(x=Year,y=num_payments, color=year.f)) + 
  theme(axis.ticks=element_blank(),
        axis.text.x=element_blank()) +
  scale_color_gdocs() + 
  guides(color = F, size = F)


exp.s$PayBin = cut(exp.s$num_payments, seq(0,max(num_payments),10))
exp.s$AmountBin = cut(exp.s$Amount, seq(0,range(exp.s$Amount)[2],3000000))

(qplot(Date, Amount, data=exp.s,
       scale_x_tufte(),
       scale_y_tufte(tot),
       size = num_payments,
       alpha = I(.8))
   + theme_fivethirtyeight())
 + scale_color_gdocs(aes(color = start_year))
)
# + scale_colour_calc()
# + geom_smooth(aes(color = start_year), method = 'lm')
# + facet_grid(AmountBin ~ YearBin, margins = T)


dummies = model.matrix(~year.f)  

a = lm(num_payments ~ dummies, exp.s)

summary(a)

abline(lm(num_payments ~ c_tot, exp.c), col="red", lwd = 2, lty = 1)


dev.off()
par(mfrow = c(2,1))

plot(exp.c$c_tot,
     exp.c$len,
     pch = 1,
     xlab = "$$$", 
     ylab = "# Days", 
     main = "Does a contract's total budget drive its length in days?",
     sub = "THIS IS ONLY FOR DEMO PURPOSES")
#col=rgb(1,0,0,.8)), maxcolorValue=255?
abline(lm(len ~ c_tot, exp.c), col="red", lwd = 2, lty = 1)
plot(exp.c$c_tot, 
     exp.c$num_payments, 
     xlab = "$$$", 
     ylab = "# of Payments", 
     main = "What about the number of payments we have to make (i.e. does a larger $ contract have more or larger payments)?",
     sub = "THIS IS ONLY FOR DEMO PURPOSES")
abline(lm(num_payments ~ c_tot, exp.c), col="red", lwd = 2, lty = 1)
#reset graphics device
dev.off()

require(ggplot2)
require(ggthemes)
qplot(exp.l$Amount, geom = 'point')
qplot(exp.l$tot, exp.l$Amount, geom = 'point') + aes(color = exp.l$Month, alpha = 0.5)
qplot(exp.l$len, exp.l$Amount, geom = 'point') + aes(size = exp.l$len_months)
qplot(exp.l$Month, exp.l$Amount)

qplot(as.numeric((exp.l$start)))
qplot(as.numeric(exp.l$end))
qplot(exp.l$len)
qplot(exp.l$num_payments)
qplot(exp.l$Day)

p <- ggplot(exp.s, aes(tot, num_payments))
p + geom_point(aes(color = start_year, format = "%Y"), size = tot) + 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), size = 2, col = 'red') +
  scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"))+ 
  theme_solarized_2(light = T) +
  scale_color_continuous_tableau() +
  ggtitle("Costs Drive Payments")
  # scale_color_grey(start = .2, end = 1)

# Boxplots for factor levels (UNEDITED) 
# ggplot(data = MYdata, aes(x = Age, y = Richness)) + 
#   geom_boxplot(aes(fill=factor(Age))) + 
#   geom_point(aes(color = factor(Age))) +
#   scale_x_continuous(breaks = c(0, 1, 3, 6, 9, 12)) +
#   scale_colour_manual(breaks = c("0", "1", "3", "6", "9", "12"),
#                       labels = c("0 month", "1 month", "3 months",
#                                  "6 months", "9 months", "12 months"),
#                       values = c("#E69F00", "#56B4E9", "#009E73", 
#                                  "#F0E442", "#0072B2", "#D55E00")) +
#   scale_fill_manual(breaks = c("0", "1", "3", "6", "9", "12"),
#                     labels = c("0 month", "1 month", "3 months",
#                                "6 months", "9 months", "12 months"),
#                     values = c("#E69F00", "#56B4E9", "#009E73", 
#                                "#F0E442", "#0072B2", "#D55E00"))
