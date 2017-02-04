# setwd("C:/Data/police")
# 
# mydata <- read.table("2009-NPMSRP-Report-Aggregate-updated-2012-06-04.csv", header=FALSE)
# read.table("mydata.csv", header=TRUE, sep=",", na.strings=".")
# p09 <- read.table("2009-NPMSRP-Report-Aggregate-updated-2012-06-04.csv", header=TRUE, sep=",", na.strings=".")
# 
# AS.NUMERIC()
# 
# summary(p09$Status)
# sum(sum(p09$Status=="Resignation"),sum(p09$Type=="Settlement"), sum(p09$Type=="Settlement"))
# 


#after I got help:
# install.packages("stringr")
# library(stringr)

setwd("E:/Data/police")
pm <- read.csv("NPMRP Data 2009-2010.csv", strip.white=TRUE) #stringsAsFactors=FALSE,
View(pm)
library(dplyr)
byjur <- group_by(pm, Jurisdiction)
summary = summarise(byjur,n=n())
# now rename columns to what choroplethr needs
colnames(summary) = c("jurisdiction", "value")
pairs(~ Year + Jurisdiction + Type + Status, pm)
summary(pm$Status)

pm <- pm[,-5]
head(pm)

str(pm$Status)
str(pm$Type)
labst<-labels(pm$Type)
labss<-labels(pm$Status)



head(labst)

pm$Type <- as.numeric(pm$Type)
#note that status and type are already levels
pm$Status %in% c("Charge", "Charged")
!(pm$Status %in% c("Charge", "Charged"))
pm$Guilt <- as.numeric(!(pm$Status %in% c("Charge", "Charged")))
head(pm)



#1
vect <- as.character(pm$Jurisdiction) #c("aaa, FFF, usa", "BBB, usa", "aaa, GGG, usa", "HHH, usa")
num.seqs <- unlist(lapply(strsplit(vect, ","), length))<2
num.seqs3 <- unlist(lapply(strsplit(vect, ","), length))>2
num.seqs2 <- unlist(lapply(strsplit(vect, ","), length))==2

state <- vect
library(stringr)
state[num.seqs3] <- str_replace(vect[num.seqs3], '.+, (.+?),.+', '\\3')
state[num.seqs2] <- str_replace(vect[num.seqs2], '(.+?),.+', '\\2')
state[num.seqs] <- str_replace(vect[num.seqs], '(.+?),.+', '\\1')
pm$State <-  state #str_replace(state[num.seqs3], '.+, (.+?),.+', '\\1')


#2
samp <- pm$Jurisdiction #c('foo, USA', 'fooville, foo, USA', 'USA')
state <- str_replace(samp[str_detect(samp, '(.+,)*(.+),.+')],'((.+,)*(.+)),.+','\\3') #change instance #
pm$State <-state


