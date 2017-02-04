require(RCurl)
require(dplyr)

# ingest from source (NOT a live connection)
df <- read.csv("Z:\\DA&DS\\Data Scientists Only\\Jankity Reporting Update\\daily_send_historical.csv")
# setwd("")

# always look at the data (check for blank/NA columns like "X" )
glimpse(df)

 # this is the file name that will be on your desktop and in the /dropoff folder
write_string <- "daily_send_historical_out"

# this is the file path to which we write, below
file_name <- paste("Z:\\DA&DS\\Data Scientists Only\\Jankity Reporting Update\\daily_send_historical_out.csv")

# format dat date
df$EVENT_DATE <- as.Date(df$EVENT_DATE, format = '%m/%d/%Y')
df$EVENT_DATE <- as.Date(df$EVENT_DATE, format = '%Y-%m-%d')

# write csv to desktop, with no rowname (numbers) and specify an NA value for NZ
write.csv(df, file_name, row.names = FALSE, na = "NA")

# ftp upload to infosphere dropoff
ftpUpload(file_name, 
          paste("sftp://infosphere.imm.com/archive/dropoff/client_tracfone/", write_string, ".csv", sep=""), 
          userpwd = "dgrabowski:dr0p123")
