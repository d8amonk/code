ics_NT$BRAND <- "Net10"
ics_ST$BRAND <- "Straight Talk"
ics_TW$BRAND <- "Total Wireless"
ics_TF$BRAND <- "Tracfone"

setwd("c:/Users/jvangeete/Desktop/")

write.csv(ics_NT, "NT_MAs.csv", row.names = FALSE)
write.csv(ics_ST, "ST_MAs.csv", row.names = FALSE)
write.csv(ics_TW, "TW_MAs.csv", row.names = FALSE)
write.csv(ics_TF, "TF_MAs.csv", row.names = FALSE)

