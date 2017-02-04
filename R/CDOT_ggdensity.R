//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
///       ***THIS IS THE PLOTTING SECTION***       ///
///                                                ///  
///                                                ///
///     YOU NEED TO LOAD THE M_ OBJECTS!!!      ///
//////////////////////////////////////////////////////
//////////////////////////////////////////////////////
require(dplyr)
require(plyr)
qplot(Amount, num_payments, data = exp.s, geom = "violin")  
  

library(ggplot2)
library(gridExtra)
require(ggthemes)
require(lubridate)
options(scipen=999)

# set.seed(10005) 

# date <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
# amount <- c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
# N <- as.factor(c(rep(1, 1500), rep(2, 1500)))
# xy <- data.frame(date, amount, N)

#DATA
exp.z <- read.csv("C:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/.csv")
exp.h <- read.csv("C:/Users/Jeffrey/Google Drive/CDOT/Data/On Deck/m_01_14.csv", header = F)



colnames(exp.s) <- c("Year", "Month", "MonthCount", "Amount")
exp.s$Day = 1

exp.s$Date <- paste(exp.s$Year,0,exp.s$Month,0,exp.s$Day,sep = "")
exp.s$Date <- as.Date(exp.s$Date,"%Y%m%d")

date <- as.Date(exp.s$Date, "%Y-%m-%d")
amount <- exp.s$Amount
N <- exp.s$MonthCount
Y <- exp.s$Year

#make same levels for 2 variables 
mylevels <- as.character(sort(unique(c(year(exp.s$Date),exp.s$start_year))))



xy <- data.frame(date, amount, N, Y)


x <- exp.s %>% 
  group_by(Year) %>% 
  summarise(avg_ct = median(MonthCount),
            hi_ct = max(MonthCount),
            low_ct = min(MonthCount))
require(ggplot2)

ggplot(xy, aes(x = , y = N)) + geom_boxplot()

# HISTOGRAMS
#counts on y-axis       
h1 <- ggplot(xy, aes(N))
# h2 <- ggplot(xy, aes(amount)) + geom_histogram(binwidth=1000) 
h1 <- h1 + geom_density()
h1
#density on y-axis
h2 <- ggplot(xy, aes(x=N, col = date)) + geom_density()
h2 <- h2 + geom_histogram(aes(y = ..density..))
h2
h2  + geom_density()
grid.arrange(h1, h2, nrow=1)


#DENSITY PLOTS
d1 <- ggplot(xy, aes(N)) + geom_density(aes(color = date))
d1
#histogram with density line overlaid
d2 <- ggplot(xy, aes(x = N)) + geom_histogram(aes(y = ..density..), color="black", fill=NA, binwidth = 5)
d2


d3 <- d2 + geom_density(color="red", size = 2)
d3
#split and color by third variable, alpha fades the color a bit
d4 <-ggplot(xy, 
            aes(N, fill = as.factor(year(exp.s$Date)))) + 
  geom_density(alpha = 0.3) + 
  theme_fivethirtyeight() + 
  scale_colour_fivethirtyeight()

d4
grid.arrange(d1, d3, d4, ncol=1)


#jitter plot, color by year of payment
b1<-ggplot(xy, aes(date, amount,
                   alpha=0.8, color=mylevels, size = N))  + 
  geom_jitter() + 
  scale_color_identity() +
  theme_classic() +
  theme(legend.position="none")
  # theme(axis.title.x=element_text("Year"))
b1

#jitter, color by contract start year
b2<-ggplot(xy, aes(mydateYYYY, amount,
                   alpha=0.9, color=start_year, size = N))  + 
  geom_jitter() + 
  scale_color_identity()  +
  theme_classic() +
  theme(legend.position="none") 
b2

#boxplot, color by year of payment

# my.ggplot + coord_cartesian(ylim=c(lower.fence, upper.fence))

b3<-ggplot(xy, aes(mydateYYYY, amount, fill = start_year)) +  geom_boxplot()
ylim1 = boxplot.stats(amount)$stats[c(1,4)]
b3<-b3 + coord_cartesian(ylim = ylim1*2) +
  scale_fill_identity("legend") +
  theme_classic() 
#   theme(axis.text.x=element_blank(),
#         axis.title.x=element_blank())
b3




grid.arrange(b1, b2, b3, ncol=1)

#BACK UP BOXPLOTS
# b1<-ggplot(xy, aes(x = as.factor(year(date)), amount)) + 
#   geom_boxplot(aes(fill = as.factor(year(date)))) +
#   theme(legend.position = "none") + scale_color_hc() +
#   scale_y_discrete()
# 
# #jitter plot
# b2<-ggplot(xy, aes(x = as.factor(year(date)), amount))  + 
#   geom_jitter(alpha=I(.6), aes(color=N)) +
#   theme(legend.position = "none")+
#   scale_y_tufte()
# 
# grid.arrange(b1, b2, nrow=1) 

#rug plot - WATCH OUT IT'S STICKY!
r1 <- ggplot(xy, aes(date,amount, size = N, alpha = .6))  + geom_point() + geom_rug(col="darkred", alpha=.1)
r1

#placeholder plot - prints nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

#scatterplot of x and y variables
scatter <- ggplot(xy,aes(date, amount)) + 
  geom_point(aes(color=N, size = date, alpha = .8)) + 
  scale_color_manual(values = c("orange", "purple")) + 
  theme(legend.position=c(1,1),legend.justification=c(1,1)) 

#marginal density of x - plot on top
plot_top <- ggplot(xy, aes(date, fill=N)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none")

#marginal density of y - plot on the right
plot_right <- ggplot(xy, aes(amount, fill=N)) + 
  geom_density(alpha=.5) + 
  coord_flip() + 
  scale_fill_manual(values = c("orange", "purple")) + 
  theme(legend.position = "none") 

#arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
