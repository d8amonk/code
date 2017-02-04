# install.packages(c("choroplethr", "choroplethrMaps"))
library(choroplethr)
library(choroplethrMaps)
library(dplyr)

df <- as.data.frame(
  read.csv("c:/Users/jvangeete/Desktop/Sandbox/Asks/mike/st_dr_walmart_v2.csv")
)

# doign stuff with mike
glimpse(df)
View(df)
df <- df[complete.cases(df), ]
cor(df$Walmarts, df$Sales)
cor(df$Walmarts, df$HHI)
summary(lm(data = df, ))

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# trim leading and trailing whitespace off brand
df$CPA <- as.numeric(trim(df$CPA))
df$Conv.Rate <- as.numeric(trim(df$Conv.Rate))
summary(df)
df <- df[complete.cases(df),]
df <- rename(df, region = State)

# state.abb[grep(df$df, state.name)]
df$region <- state.abb[match(df$region, state.name)]

table(df$Brand)
table(df$region)

df_nt <- filter(df, Brand == "Net10")
df_st <- filter(df, Brand == "Straight Talk")
df_tw <- filter(df, Brand == "Total Wireless")
df_tf <- filter(df, Brand == "Tracfone")

df_nt_cr <- rename(df_nt, value = Conv.Rate)
df_st_cr <- rename(df_st, value = Conv.Rate)
df_tw_cr <- rename(df_tw, value = Conv.Rate)
df_tf_cr <- rename(df_tf, value = Conv.Rate)

state_choropleth(df_nt_cr)
