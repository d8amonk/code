require(RGoogleAnalytics)
require(lubridate)
require(RCurl)
require(nzr)

nzConnect("drew", "Kiopses1", "192.168.100.5", "client_roku", force=TRUE)
# nzShowTables() # This shows you all the tables in the DB you selected above

nzShowTables()

HBO_0228 <- read.csv("Z:/wireless/Roku/ESN Data/HBO wrap/Roku_HBO-ESNData_020116_022816_export.csv", header = T)
HBO_0228$hbo_trial_start_date <- as.Date(HBO_0228$hbo_trial_start_date, format="%m/%d/%Y")
HBO_0228$EVENT_TS <- strptime(HBO_0228$EVENT_TS, format="%m/%d/%Y  %H:%M")
HBO_0228$HASHED_ESN <- as.character(HBO_0228$HASHED_ESN)

HBO_0410 <- read.csv("Z:/wireless/Roku/ESN Data/HBO wrap/Roku_HBO-ESNData_030116_041016_export.csv", header= T)
HBO_0410$hbo_trial_start_date <- as.Date(HBO_0410$hbo_trial_start_date, format="%m/%d/%Y")
HBO_0410$EVENT_TS <- strptime(HBO_0410$EVENT_TS, format="%m/%d/%Y  %H:%M")
HBO_0410$HASHED_ESN <- as.character(HBO_0410$HASHED_ESN)


HBO_grouped <- rbind(HBO_0228, HBO_0410)

head(HBO_grouped)
summary(HBO_grouped)

HBO_grouped_file <- "C:/Users/aknorr/Documents/R/grouped_hbo_ESN.csv"

write.csv(HBO_grouped, file = HBO_grouped_file, row.names = F)

ftpUpload(HBO_grouped_file,
  "sftp://infosphere.imm.com/archive/dropoff/client_roku/grouped_hbo_ESN.csv", userpwd = "dgrabowski:dr0p123"
  )
