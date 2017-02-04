##Automating Bid Amounts

##Testing with this data set
# ST_SEM <- read.csv("C:/Users/aknorr/Documents/Projects/SEM experiments/ST Dads bid fun.csv", header = T)
# df <- read.csv("C:/Users/aknorr/Documents/Projects/SEM experiments/ST Dads bid fun.csv", header = T)

df <- read.csv("\\\\fileserver/Company/DA&DS/Data Scientists Only/SEM Experiment Results/Display Data/TTD display data 6.20 - current.csv")

##Function Time!!

Bid_sys <- function(cpa, df, csv_name){
  
  library(dplyr)
  
  df$Ad.group <- as.character(df$Ad.group)
  
  df$Type <- ifelse(test = grepl("\\[", df$Keyword), yes = "Exact", no = ifelse( test = grepl("\\+",df$Keyword), yes ="Broad", no ="Phrase"))
  df$Type <- as.factor(df$Type)
  
  df_1 <- filter(df, as.integer(df$Type)==1)
  df_2 <- filter(df, as.integer(df$Type)==2)
  df_3 <- filter(df, as.integer(df$Type)==3)
  
  Broad_wt <- sum(df_1$Conversions)/ sum(df$Conversions)
  Exact_wt <- sum(df_2$Conversions)/ sum(df$Conversions)
  Phrase_wt <- sum(df_3$Conversions)/ sum(df$Conversions)
  
  df_1$Percentile <- percent_rank(df_1$Conversions)
  df_2$Percentile <- percent_rank(df_2$Conversions)
  df_3$Percentile <- percent_rank(df_3$Conversions)
  
  df_1$Conv_weight <- df_1$Conversions/sum(df_1$Conversions)
  df_2$Conv_weight <- df_2$Conversions/sum(df_2$Conversions)
  df_3$Conv_weight <- df_3$Conversions/sum(df_3$Conversions)
 
  df_1$Initial_bid <- ifelse(test = df_1$Conv..rate > 1 | df_1$Clicks < 100, 
                                     round(df_1$Conv..rate*df_1$Conv_weight*cpa,2), 
                                     round(df_1$Conv..rate*cpa,2))
  df_2$Initial_bid <- ifelse(test = df_2$Conv..rate > 1 | df_2$Clicks < 100, 
                                     round(df_2$Conv..rate*df_2$Conv_weight*cpa,2), 
                                     round(df_2$Conv..rate*cpa,2))
  df_3$Initial_bid <- ifelse(test = df_3$Conv..rate > 1 | df_3$Clicks < 100, 
                                      round(df_3$Conv..rate*df_3$Conv_weight*cpa,2), 
                                      round(df_3$Conv..rate*cpa,2))  
  
  df_1$Type_adj <- 1+Broad_wt
  df_2$Type_adj <- 1+Exact_wt
  df_3$Type_adj <- 1+Phrase_wt
  
  df_1$Percent_adj <- 1+df_1$Percentile
  df_2$Percent_adj <- 1+df_2$Percentile
  df_3$Percent_adj <- 1+df_3$Percentile
  
  df_1$Final_bid <- df_1$Initial_bid*df_1$Type_adj*df_1$Percent_adj
  df_2$Final_bid <- df_2$Initial_bid*df_2$Type_adj*df_2$Percent_adj
  df_3$Final_bid <- df_3$Initial_bid*df_3$Type_adj*df_3$Percent_adj
  
  df_final <- rbind(df_1,df_2,df_3)
  
  write.csv(df_final, file = paste("\\\\fileserver/Company/DA&DS/Data Scientists Only/SEM Experiment Results/",csv_name,".csv",sep=""), row.names=F)
  
  
}


