library(nzr); library(dplyr); library(scales); library(lubridate); library(TTR); library(tidyr)

nzConnect("FASKHAM", "Helium7!", "192.168.100.5", "client_tracfone", force=TRUE)
# nzShowTables()

gm <- as.data.frame(nz.data.frame("GM_REPORTABLE")) %>% 
  group_by(EVENT_DATE_EST) %>% 
  mutate(row_count = n()) %>% 
  arrange(desc(EVENT_DATE_EST)) %>% 
  ungroup()

ct_check <- function(x){
  if(all(x$row_count == 4)) {
  return("All dates have 4 rows.")
    } else {
    paste("Some date(s) has issues:", 
          seq.Date(Sys.Date() - 5, Sys.Date(), by = "days"))
  #fill in with rows to find that date's date and index
    }
}

nzDisconnect()
gm$EVENT_DATE_EST <- as.Date(gm$EVENT_DATE_EST)

st <- Sys.Date() - 30
# [1] "2016-07-05"

en <- Sys.Date() - 1
# [1] "2016-08-04" 

last_30 <-  sort(as.Date(seq.Date(st, en, by = "days", origin = '1970-01-01')))
gm_30 <- sort(unique(gm$EVENT_DATE_EST)[1:30])


gm_
(date_param <- as.Date(setdiff(test_30, last_30), origin = '1970-01-01'))
# character(0) if g2g

min(as.numericdate_param))

#######################
# need to feed query_date_params to a source call to tf_seo_rscript
# thinking min(query_date_params) - 1, max(query_date_params) + 1 as params 