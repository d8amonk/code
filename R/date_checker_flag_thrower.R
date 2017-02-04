library(nzr); library(dplyr); library(scales); library(lubridate); library(TTR); library(tidyr)

nzConnect("FASKHAM", "Helium7!", "192.168.100.5", "client_tracfone", force=TRUE)
# nzShowTables()

gm_full <- arrange(as.data.frame(nz.data.frame("GM_REPORTABLE")), desc(EVENT_DATE_EST))
gm_full$EVENT_DATE_EST <- as.Date(gm_full$EVENT_DATE_EST)
# gm_temp <- gm_full[-c(4:19),]

gm <- gm_temp %>% 
  group_by(EVENT_DATE_EST) %>% 
  mutate(row_count = n()) %>% 
  arrange(desc(EVENT_DATE_EST)) %>% 
  ungroup() %>% 
  rename(Date = EVENT_DATE_EST) %>% 
  select(Date,
         row_count) %>% 
  unique()

nzDisconnect()


# all(last_30 %in% gm_30)

date_checker <- function(x){
  
  st <- Sys.Date() - 30
  en <- Sys.Date() - 1
  last_30 <-  sort(as.Date(seq.Date(st, en, by = "days", origin = '1970-01-01')))
  
  gm_30 <- sort(unique(x$Date)[1:30])
  
  date_param <- as.Date(setdiff(last_30, gm_30), origin = '1970-01-01')
  
  cat("Row Count Diagnostics...\n")
  
  if(all(x$row_count == 4)) {
    return("All dates have 4 rows.")
  } else {
    cat("These date(s) were present, but had issues:\n")
    print(
      paste(x$Date[x$row_count != 4], 
            "had",
            x$row_count[x$row_count != 4],
            "rows.")
    )
    
  }
  
  cat('\nThese dates have 0 rows...\n')
  needed_dates <- as.Date(setdiff(last_30, gm_30), origin = '1970-01-01')
  print(needed_dates)
  
  choice <- readline("Would you like to pass these missing dates to the GA Query (y/n)?")
  
  if(choice == 'y'){
    print("Retrieving dates using GA query.")
  } else {
    print("Try again sucker!")
  }
  
  
}; date_checker(gm)



#######################
# need to feed query_date_params to a source call to tf_seo_rscript
# thinking min(query_date_params) - 1, max(query_date_params) + 1 as params 