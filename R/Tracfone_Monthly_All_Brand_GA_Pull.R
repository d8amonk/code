tf.ga.request.fun <- function(ga.profiles, start_date = as.Date('2016-01-01'), end_date = as.Date('2016-03-31')) {
  
  # timeFirstDayInMonth(Sys.Date()-3)
  # Sys.Date()-3
  
  require(timeDate)
  require(RGoogleAnalytics)
  require(RGA)
  require(lubridate)
  require(RCurl)
  
  load("\\\\fileserver/Company/DA&DS/R Information/RGoogleAnalytics/rGA.token")
  
  ValidateToken(rGA.token)
  
  authorize()
  
  ga.profiles <- GetProfiles(rGA.token)
  
  full.rga.profiles <- list_profiles()
  
  table_vect <- c('ga:88935108', 'ga:84213723', 'ga:68477125', 'ga:22605248', 'ga:84151163', 'ga:60284978', 'ga:98674155', 'ga:84223902')
  name_vect <- c("Clearway.com", "Net10wireless.com", "Pagepluscellular.com", "Simplemobile.com","Straighttalk.com","Telcelamerica.com", "Totalwireless.com", "Tracfone.com")
  
  full.fun <- function(x, y) {
    
    ga.init <- Init(start_date, end_date, 
                      dimensions = c("ga:date","ga:month", "ga:deviceCategory","ga:browser", 
                                     "ga:browserVersion", "ga:operatingSystem", "ga:operatingSystemVersion"), 
                      metrics=c("ga:sessions","ga:users"),table.id = x, max.results=10000)
    
    
      
    ga.query <- QueryBuilder(ga.init)
      
    ga.data <- GetReportData(ga.query, rGA.token, split_daywise = T)
      
    ga.agg <- aggregate(cbind(sessions, users) ~ date + month + deviceCategory + browser + browserVersion + operatingSystem + operatingSystemVersion, data = ga.data, sum)
    ga.agg$date <- as.Date(ga.agg$date, "%Y%m%d")  
    # ga.url <- full.rga.profiles$website.url[full.rga.profiles$id==ga.profiles[i]]
    # ga.url <- unlist(strsplit(ga.url, split=".", fixed=T))[2]
    # ga.sheet <- paste(ga.url, "-", ga.profiles[i])
    
    ga.file <- paste("c:/Users/jvangeete/Desktop/tempdrop/Tracfone_Request_", y, ".csv", sep="")
    
    write.csv(ga.agg, file = ga.file, row.names=F)
  
  }
  mapply(full.fun, table_vect, name_vect)
}