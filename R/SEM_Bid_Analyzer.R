##Automating Bid Amounts

##Testing with this data set
ST_SEM <- read.csv("C:/Users/aknorr/Documents/Projects/SEM experiments/ST Dads bid fun.csv", header = T)
df <- read.csv("C:/Users/aknorr/Documents/Projects/SEM experiments/ST Dads bid fun.csv", header = T)


ST_SEM$Ad.group <- as.character(ST_SEM$Ad.group)

ST_SEM_Broad <- filter(ST_SEM, as.integer(ST_SEM$Type)==1)
ST_SEM_Exact <- filter(ST_SEM, as.integer(ST_SEM$Type)==2)
ST_SEM_Phrase <- filter(ST_SEM, as.integer(ST_SEM$Type)==3)

Broad_wt <- sum(ST_SEM_Broad$Conversions)/ sum(ST_SEM$Conversions)
Exact_wt <- sum(ST_SEM_Exact$Conversions)/ sum(ST_SEM$Conversions)
Phrase_wt <- sum(ST_SEM_Phrase$Conversions)/ sum(ST_SEM$Conversions)


ST_SEM_Broad$Percentile <- percent_rank(ST_SEM_Broad$Conversions)
ST_SEM_Exact$Percentile <- percent_rank(ST_SEM_Exact$Conversions)
ST_SEM_Phrase$Percentile <- percent_rank(ST_SEM_Phrase$Conversions)


ST_SEM_Broad$Conv_weight <- ST_SEM_Broad$Conversions/sum(ST_SEM_Broad$Conversions)
ST_SEM_Exact$Conv_weight <- ST_SEM_Exact$Conversions/sum(ST_SEM_Exact$Conversions)
ST_SEM_Phrase$Conv_weight <- ST_SEM_Phrase$Conversions/sum(ST_SEM_Phrase$Conversions)

ST_SEM_Broad$Initial_bid <- ifelse(test = ST_SEM_Broad$Conv..rate > 1 | ST_SEM_Broad$Clicks < 100, 
                                   round(ST_SEM_Broad$Conv..rate*ST_SEM_Broad$Conv_weight*48.67,2), 
                                   round(ST_SEM_Broad$Conv..rate*48.67,2))
ST_SEM_Exact$Initial_bid <- ifelse(test = ST_SEM_Exact$Conv..rate > 1 | ST_SEM_Exact$Clicks < 100, 
                                   round(ST_SEM_Exact$Conv..rate*ST_SEM_Exact$Conv_weight*48.67,2), 
                                   round(ST_SEM_Exact$Conv..rate*48.67,2))
ST_SEM_Phrase$Initial_bid <- ifelse(test = ST_SEM_Phrase$Conv..rate > 1 | ST_SEM_Phrase$Clicks < 100, 
                                   round(ST_SEM_Phrase$Conv..rate*ST_SEM_Phrase$Conv_weight*48.67,2), 
                                   round(ST_SEM_Phrase$Conv..rate*48.67,2))                                   


ST_SEM_Broad$Type_adj <- 1+Broad_wt
ST_SEM_Exact$Type_adj <- 1+Exact_wt
ST_SEM_Phrase$Type_adj <- 1+Phrase_wt

ST_SEM_Broad$Percent_adj <- 1+ST_SEM_Broad$Percentile
ST_SEM_Exact$Percent_adj <- 1+ST_SEM_Exact$Percentile
ST_SEM_Phrase$Percent_adj <- 1+ST_SEM_Phrase$Percentile

ST_SEM_Broad$Final_bid <- ST_SEM_Broad$Initial_bid*ST_SEM_Broad$Type_adj*ST_SEM_Broad$Percent_adj
ST_SEM_Exact$Final_bid <- ST_SEM_Exact$Initial_bid*ST_SEM_Exact$Type_adj*ST_SEM_Exact$Percent_adj
ST_SEM_Phrase$Final_bid <- ST_SEM_Phrase$Initial_bid*ST_SEM_Phrase$Type_adj*ST_SEM_Phrase$Percent_adj


##Function Time!!

Bid_sys_test <- function(cpa, df, csv_name){
  
  library(dplyr)
  
  df$Ad.group <- as.character(df$Ad.group)
  
  df$Type <- ifelse(test = grepl("\\[", df$Keyword), yes = "Exact", no = ifelse( test = grepl("\\+",df$Keyword), yes ="Broad", no ="Phrase"))
  df$Type <- as.factor(df$Type)

  for (i in 1:max(as.integer(df$Type))) {
    if (i == 1){
      df_1 <- filter(df, as.integer(df$Type)==1)
      df1_conv_wt <- sum(df_1$Conversions)/ sum(df$Conversions)
      df_1$Percentile <- percent_rank(df_1$Conversions)
      df_1$Conv_weight <- df_1$Conversions/sum(df_1$Conversions)
      df_1$Initial_bid <- ifelse(test = df_1$Conv..rate > 1 | df_1$Clicks < 100, 
                                 round(df_1$Conv..rate*df_1$Conv_weight*cpa,2), 
                                 round(df_1$Conv..rate*cpa,2))
      df_1$Type_adj <- 1+df1_conv_wt
      df_1$Percent_adj <- 1+df_1$Percentile
      df_1$Final_bid <- df_1$Initial_bid*df_1$Type_adj*df_1$Percent_adj
      df_final <<- df_1
    }
    if (i == 2){
      df_2 <- filter(df, as.integer(df$Type)==2)
      df2_conv_wt <- sum(df_2$Conversions)/ sum(df$Conversions)
      df_2$Percentile <- percent_rank(df_2$Conversions)
      df_2$Conv_weight <- df_2$Conversions/sum(df_2$Conversions)
      df_2$Initial_bid <- ifelse(test = df_2$Conv..rate > 1 | df_2$Clicks < 100, 
                                 round(df_2$Conv..rate*df_2$Conv_weight*cpa,2), 
                                 round(df_2$Conv..rate*cpa,2))
      df_2$Type_adj <- 1+df2_conv_wt
      df_2$Percent_adj <- 1+df_2$Percentile
      df_2$Final_bid <- df_2$Initial_bid*df_2$Type_adj*df_2$Percent_adj
      df_final <<- rbind(df_1, df_2)
    }
    if (i == 3){
      df_3 <- filter(df, as.integer(df$Type)==3)
      df3_conv_wt <- sum(df_3$Conversions)/ sum(df$Conversions)
      df_3$Percentile <- percent_rank(df_3$Conversions)
      df_3$Conv_weight <- df_3$Conversions/sum(df_3$Conversions)
      df_3$Initial_bid <- ifelse(test = df_3$Conv..rate > 1 | df_3$Clicks < 100, 
                                 round(df_3$Conv..rate*df_3$Conv_weight*cpa,2), 
                                 round(df_3$Conv..rate*cpa,2))
      df_3$Type_adj <- 1+df3_conv_wt
      df_3$Percent_adj <- 1+df_3$Percentile
      df_3$Final_bid <- df_3$Initial_bid*df_3$Type_adj*df_3$Percent_adj
      df_final <<- rbind(df_1,df_2, df_3)
    }
  }
  
  write.csv(df_final, file = paste("Y:/DA&DS/Data Scientists Only/SEM White Paper/",csv_name,".csv",sep=""), row.names=F)
  
}


