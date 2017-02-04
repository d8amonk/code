#R Script for Automating Search Console Data#


rseo.pull.param <- function(start.date = as.character(today() - 10)) {
  
# INITIALIZATION ----------------
  require(googleAuthR)
  require(lubridate)
  require(RCurl)
  require(nzr)
  require(searchConsoleR)
  
  nzConnect("drew", "Kiopses1", "192.168.100.5", "GOOGLE_ANALYTICS_DATA", force = TRUE)
  
  seo.token <- gar_auth()
  
  end.date <- as.character(today() - 3)
  
  roku.seo.sites <- list_websites()
  
  siteurl <- c("https://www.roku.com/", "https://support.roku.com/", "https://my.roku.com/", "http://forums.roku.com/", 
                                                        "https://blog.roku.com/", "https://channelstore.roku.com/", "https://www.straighttalk.com/", 
                                                        "http://www.tracfone.com/", "https://www.totalwireless.com/") 
  description <- c("main", "supsec", "my", "forum", "blog", "channel", "st_main", "tf_main", "tw_main")
  SITE <- c("Main", "Support Secured", "My", "Forum", "Blog", "Channel", "Main", "Main", "Main")
  ftpupload1 <- c("roku", "roku", "roku", "roku", "roku", "roku", "st", "tf", "tw")
  ftpupload2 <- c("main", "supsec", "my", "forum", "blog", "channel", "main", "main", "main")
  roku.seo.sites.filter <- data.frame(siteurl, description, SITE, ftpupload1, ftpupload2)
  roku.seo.sites.filter$siteurl <- as.character(roku.seo.sites.filter$siteurl)
  roku.seo.sites.filter$description <- as.character(roku.seo.sites.filter$description)
  roku.seo.sites.filter$SITE <- as.character(roku.seo.sites.filter$SITE)
  roku.seo.sites.filter$ftpupload1 <- as.character(roku.seo.sites.filter$ftpupload1)
  roku.seo.sites.filter$ftpupload2 <- as.character(roku.seo.sites.filter$ftpupload2)
  
#Full---------------------  
cat("Running Full pulls\n\n") 
full.fun <- function(x,y,z,a,b) { 
  data.full<- search_analytics(x, start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
        searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames = F, dimensionFilterExp = "country==USA"
      )
      
    try(colnames(data.full)[1] <- "EVENT_TS")
  
    data.full <-
      cbind(SITE= z, data.full)
    
    data.full <- try(data.full[,c(2,1,3:11)])
    
    data.full.file <-
      paste("D://imm_automation/R_Files/", "full_",y,".csv", sep ='')
    
    write.csv(data.full, file = data.full.file, row.names = F)

    #ftpUpload(
      #data.full.file, paste("sftp://infosphere.imm.com/archive/dropoff/client_roku/",a,"_seo_full_",b,".csv", sep=''), userpwd = "dgrabowski:dr0p123"
    #)
}
mapply(full.fun, roku.seo.sites.filter$siteurl, roku.seo.sites.filter$description, roku.seo.sites.filter$SITE, roku.seo.sites.filter$ftpupload1, roku.seo.sites.filter$ftpupload2) 
#Page-----------------------  
cat("Running Page pulls\n\n")
page.fun <- function(x,y,z,a,b){  
  data.page <-search_analytics(x, start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
        searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
          F, dimensionFilterExp = "country==USA"
      )
    try(colnames(data.page)[1] <- "EVENT_TS")
    
    data.page <-
      cbind(SITE= z, data.page)
    
    data.page <- try(data.page[,c(2,1,3:10)])
    
    data.page.file <-
      paste("D://imm_automation/R_Files/","page_",y,".csv", sep ='')
    
    write.csv(data.page, file = data.page.file, row.names = F)
    
    #ftpUpload(
      #data.page.file, paste("sftp://infosphere.imm.com/archive/dropoff/client_roku/",a, "_seo_page_",b,".csv", sep=''), userpwd = "dgrabowski:dr0p123"
    #) 
}
mapply(page.fun, roku.seo.sites.filter$siteurl, roku.seo.sites.filter$description, roku.seo.sites.filter$SITE, roku.seo.sites.filter$ftpupload1, roku.seo.sites.filter$ftpupload2)
#Query------------------------- 
cat("Running Query pulls\n\n")
query.fun <- function(x,y,z,a,b){    
  data.query <- search_analytics(x, start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
        searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
          F, dimensionFilterExp = "country==USA"
      )
    try(colnames(data.query)[1] <- "EVENT_TS")
    
    data.query <-
      cbind(SITE = z, data.query)
    
    data.query <- try(data.query[,c(2,1,3:10)])
    
    data.query.file <-
      paste("D://imm_automation/R_Files/","query_",y,".csv", sep ='')
    
    write.csv(data.query, file = data.query.file, row.names = F)
    
    #ftpUpload(
      #data.query.file, paste("sftp://infosphere.imm.com/archive/dropoff/client_roku/",a, "_seo_query_",b,".csv", sep=''), userpwd = "dgrabowski:dr0p123"
    #) 
    
}
mapply(query.fun, roku.seo.sites.filter$siteurl, roku.seo.sites.filter$description, roku.seo.sites.filter$SITE, roku.seo.sites.filter$ftpupload1, roku.seo.sites.filter$ftpupload2)
# NET10 BRAND SEO QUERIES  - http://www.net10wireless.com/ + http://www.net10.com/ ----------------
cat("NET10 BRAND SEO QUERIES  - http://www.net10wireless.com/ + http://www.net10.com/\n\n")  

nt.wireless.seo.full.main <-
  search_analytics(
    "http://www.net10wireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
    searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
      F, dimensionFilterExp = "country==USA"
  )
try(colnames(nt.wireless.seo.full.main)[1] <- "EVENT_TS")


nt.wireless.seo.page.main <-
  search_analytics(
    "http://www.net10wireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
    searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
      F, dimensionFilterExp = "country==USA"
  )
try(colnames(nt.wireless.seo.page.main)[1] <- "EVENT_TS")


nt.wireless.seo.query.main <-
  search_analytics(
    "http://www.net10wireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
    searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
      F, dimensionFilterExp = "country==USA"
  )
try(colnames(nt.wireless.seo.query.main)[1] <- "EVENT_TS")


nt.wireless.seo.full.main <-
  cbind(SITE = "Main", nt.wireless.seo.full.main)
nt.wireless.seo.full.main <- try(nt.wireless.seo.full.main[,c(2,1,3:11)])

nt.wireless.seo.page.main <-
  cbind(SITE = "Main", nt.wireless.seo.page.main)
nt.wireless.seo.page.main <- try(nt.wireless.seo.page.main[,c(2,1,3:10)])

nt.wireless.seo.query.main <-
  cbind(SITE = "Main", nt.wireless.seo.query.main)
nt.wireless.seo.query.main <- try(nt.wireless.seo.query.main[,c(2,1,3:10)])


nt.com.seo.full.main <-
  search_analytics(
    "http://www.net10.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
    searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
      F, dimensionFilterExp = "country==USA"
  )
try(colnames(nt.com.seo.full.main)[1] <- "EVENT_TS")


nt.com.seo.page.main <-
  search_analytics(
    "http://www.net10.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
    searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
      F, dimensionFilterExp = "country==USA"
  )
try(colnames(nt.com.seo.page.main)[1] <- "EVENT_TS")


nt.com.seo.query.main <-
  search_analytics(
    "http://www.net10.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
    searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
      F, dimensionFilterExp = "country==USA"
  )
try(colnames(nt.com.seo.query.main)[1] <- "EVENT_TS")


nt.com.seo.full.main <-
  cbind(SITE = "Main", nt.com.seo.full.main)
nt.com.seo.full.main <- try(nt.com.seo.full.main[,c(2,1,3:11)])

nt.com.seo.page.main <-
  cbind(SITE = "Main", nt.com.seo.page.main)
nt.com.seo.page.main <- try(nt.com.seo.page.main[,c(2,1,3:10)])

nt.com.seo.query.main <-
  cbind(SITE = "Main", nt.com.seo.query.main)
nt.com.seo.query.main <- try(nt.com.seo.query.main[,c(2,1,3:10)])


nt.seo.full.main <- rbind(nt.wireless.seo.full.main, nt.com.seo.full.main)
nt.seo.page.main <- rbind(nt.wireless.seo.page.main, nt.com.seo.page.main)
nt.seo.query.main <- rbind(nt.wireless.seo.query.main, nt.com.seo.query.main)


full.nt.main.file <-
  "D://imm_automation/R_Files/full_nt_main.csv"
page.nt.main.file <-
  "D://imm_automation/R_Files/page_nt_main.csv"
query.nt.main.file <-
  "D://imm_automation/R_Files/query_nt_main.csv"


write.csv(nt.seo.full.main, file = full.nt.main.file, row.names = F)
write.csv(nt.seo.page.main, file = page.nt.main.file, row.names = F)
write.csv(nt.seo.query.main, file = query.nt.main.file, row.names = F)


#ftpUpload(
  #full.nt.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/nt_seo_full_main.csv", userpwd = "dgrabowski:dr0p123"
#)
#ftpUpload(
  #page.nt.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/nt_seo_page_main.csv", userpwd = "dgrabowski:dr0p123"
#)
#ftpUpload(
  #query.nt.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/nt_seo_query_main.csv", userpwd = "dgrabowski:dr0p123"
#)  

#######################################################

# NETEZZA LOAD QUERY CALL ----------------  
  print("Running SEO Load Script")
  
  call <-
    paste("call LOAD_GA_ROKU_SEO_DATA('", start.date, "','", end.date, "', 1)", sep ="")
  nzQuery(call)
  
  print("SEO Load Script Complete")
  
  nzDisconnect()
  
  #####
}