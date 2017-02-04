#R Google Data Pull to Netezza Storage

rga.pull <- function(start.date = as.character(today() - 7)) {

# INITIALIZATION -----    
  
  require(RGoogleAnalytics)
  require(lubridate)
  require(RCurl)
  require(nzr)
  nzConnect("drew", "Kiopses1", "192.168.100.5", "GOOGLE_ANALYTICS_DATA", force =
              TRUE)
  
  load("D://imm_automation/R_Scripts/rGA.token")
  ValidateToken(rGA.token)
  
  end.date <- as.character(today())
  
  ga.profiles <- GetProfiles(rGA.token)
  
  ###############################################################
  
  #TRACFONE QUERIES#
  
# NET10 BRAND -----
  nt.brand.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:adContent", "ga:campaign", "ga:medium", "ga:source"
      ),
      metrics = c("ga:users"), max.results = 10000, sort =
        "ga:date", table.id = "ga:64396276"
    )
  nt.brand.query <- QueryBuilder(nt.brand.init)
  print("NT-Brand")
  nt.brand.dat <-
    try(GetReportData(nt.brand.query, rGA.token, split_daywise = TRUE))
  nt.brand.dat$date <-
    paste(
      substr(nt.brand.dat$date, 1, 4) ,"-", substr(nt.brand.dat$date, 5, 6),"-", substr(nt.brand.dat$date, 7, 8), sep =
        ""
    )
  colnames(nt.brand.dat) <-
    c("EVENT_TS", "ADCONTENT", "CAMPAIGN", "MEDIUM", "SOURCE", "USERS")
  write.csv(nt.brand.dat, file = "D://imm_automation/R_Files/NET10-Brand.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/NET10-Brand.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/NET10-Brand.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# NET10 STORE -----  
  nt.store.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date"),
      metrics = c("ga:sessions", "ga:transactions"), max.results = 10000, sort =
        "ga:date", table.id = "ga:34660956"
    )
  nt.store.query <- QueryBuilder(nt.store.init)
  print("NT-STORE")
  nt.store.dat <- try(GetReportData(nt.store.query, rGA.token))
  nt.store.dat$date <-
    paste(
      substr(nt.store.dat$date, 1, 4) ,"-", substr(nt.store.dat$date, 5, 6),"-", substr(nt.store.dat$date, 7, 8), sep =
        ""
    )
  colnames(nt.store.dat) <- c("EVENT_TS", "SESSIONS", "TRANSACTIONS")
  write.csv(nt.store.dat, file = "D://imm_automation/R_Files/NET10-Store.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/NET10-Store.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/NET10-Store.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# NET10 SPECIALS -----   
  nt.spec.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:adContent", "ga:campaign", "ga:medium", "ga:pagePath", "ga:source"
      ),
      metrics = c(
        "ga:sessions", "ga:transactionRevenue", "ga:transactions", "ga:visits"
      ), max.results = 10000, sort = "ga:date", table.id = "ga:101355912"
    )
  nt.spec.query <- QueryBuilder(nt.spec.init)
  print("NT-SPECIALS")
  nt.spec.dat <- try(GetReportData(nt.spec.query, rGA.token))
  nt.spec.dat$date <-
    paste(
      substr(nt.spec.dat$date, 1, 4) ,"-", substr(nt.spec.dat$date, 5, 6),"-", substr(nt.spec.dat$date, 7, 8), sep =
        ""
    )
  colnames(nt.spec.dat) <-
    c(
      "EVENT_TS", "ADCONTENT", "CAMPAIGN", "MEDIUM", "PAGE", "SOURCE", "SESSIONS", "REVENUE", "TRANSACTIONS", "VISITS"
    )
  write.csv(nt.spec.dat, file = "D://imm_automation/R_Files/NET10-Specials.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/NET10-Specials.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/NET10-Specials.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# STRAIGHT TALK BRAND ----- 
  
  st.brand.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date", "ga:campaign", "ga:medium", "ga:source"),
      metrics = c("ga:users", "ga:sessions"), max.results = 10000, sort =
        "ga:date", table.id = "ga:17266032"
    )
  st.brand.query <- QueryBuilder(st.brand.init)
  print("ST-BRAND")
  Sys.sleep(5)
  st.brand.dat <-
    GetReportData(st.brand.query, rGA.token, split_daywise = TRUE)
  st.brand.dat$date <-
    paste(
      substr(st.brand.dat$date, 1, 4) ,"-", substr(st.brand.dat$date, 5, 6),"-", substr(st.brand.dat$date, 7, 8), sep =
        ""
    )
  colnames(st.brand.dat) <-
    c("EVENT_TS", "CAMPAIGN", "MEDIUM", "SOURCE", "USERS", "SESSIONS")
  write.csv(st.brand.dat, file = "D://imm_automation/R_Files/ST-Brand.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/ST-Brand.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/ST-Brand.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# STRAIGHT TALK STORE ----- 
  
  st.store.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date"),
      metrics = c("ga:sessions", "ga:transactions"), max.results = 10000, sort =
        "ga:date", table.id = "ga:34664200"
    )
  st.store.query <- QueryBuilder(st.store.init)
  print("ST-STORE")
  st.store.dat <- try(GetReportData(st.store.query, rGA.token))
  st.store.dat$date <-
    paste(
      substr(st.store.dat$date, 1, 4) ,"-", substr(st.store.dat$date, 5, 6),"-", substr(st.store.dat$date, 7, 8), sep =
        ""
    )
  colnames(st.store.dat) <- c("EVENT_TS", "SESSIONS", "TRANSACTIONS")
  write.csv(st.store.dat, file = "D://imm_automation/R_Files/ST-Store.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/ST-Store.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/ST-Store.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# STRAIGHT TALK SPECIALS -----   
  st.spec.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:adContent", "ga:campaign", "ga:medium", "ga:pagePath", "ga:source"
      ),
      metrics = c(
        "ga:sessions", "ga:transactionRevenue", "ga:transactions", "ga:visits"
      ), max.results = 10000, sort = "ga:date", table.id = "ga:94928103"
    )
  st.spec.query <- QueryBuilder(st.spec.init)
  print("ST-SPECIALS")
  st.spec.dat <-
    try(GetReportData(st.spec.query, rGA.token, paginate_query = TRUE))
  st.spec.dat$date <-
    paste(
      substr(st.spec.dat$date, 1, 4) ,"-", substr(st.spec.dat$date, 5, 6),"-", substr(st.spec.dat$date, 7, 8), sep =
        ""
    )
  colnames(st.spec.dat) <-
    c(
      "EVENT_TS", "ADCONTENT", "CAMPAIGN", "MEDIUM", "PAGE", "SOURCE", "SESSIONS", "REVENUE", "TRANSACTIONS", "VISITS"
    )
  write.csv(st.spec.dat, file = "D://imm_automation/R_Files/ST-Specials.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/ST-Specials.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/ST-Specials.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TRACFONE BRAND ----- 
  
  tf.brand.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:campaign", "ga:medium", "ga:source", "ga:adContent"
      ),
      metrics = c("ga:users"), max.results = 10000, sort =
        "ga:date", table.id = "ga:650911"
    )
  tf.brand.query <- QueryBuilder(tf.brand.init)
  print("TF-Brand")
  
  tf.brand.dat <-
    try(GetReportData(tf.brand.query, rGA.token, split_daywise = TRUE))
  tf.brand.dat$date <-
    paste(
      substr(tf.brand.dat$date, 1, 4) ,"-", substr(tf.brand.dat$date, 5, 6),"-", substr(tf.brand.dat$date, 7, 8), sep =
        ""
    )
  colnames(tf.brand.dat) <-
    c("EVENT_TS", "CAMPAIGN", "MEDIUM", "SOURCE", "ADCONTENT", "USERS")
  write.csv(tf.brand.dat, file = "D://imm_automation/R_Files/TF-Brand.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/TF-Brand.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/TF-Brand.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TRACFONE STORE -----   
  tf.store.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:campaign", "ga:source", "ga:eventAction", "ga:eventCategory"
      ),
      metrics = c(
        "ga:sessions", "ga:visits", "ga:transactions", "ga:transactionRevenue"
      ), max.results = 10000, sort = "ga:date", table.id = "ga:34662915"
    )
  tf.store.query <- QueryBuilder(tf.store.init)
  print("TF-STORE")
  tf.store.dat <-
    try(GetReportData(tf.store.query, rGA.token, split_daywise = TRUE))
  tf.store.dat$date <-
    paste(
      substr(tf.store.dat$date, 1, 4) ,"-", substr(tf.store.dat$date, 5, 6),"-", substr(tf.store.dat$date, 7, 8), sep =
        ""
    )
  colnames(tf.store.dat) <-
    c(
      "EVENT_TS", "CAMPAIGN", "SOURCE", "EVENT_ACTION", "EVENT_CATEGORY", "SESSIONS", "VISITS", "TRANSACTIONS", "REVENUE"
    )
  write.csv(tf.store.dat, file = "D://imm_automation/R_Files/TF-Store.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/TF-Store.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/TF-Store.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TRACFONE SPECIALS -----   
  tf.spec.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:campaign", "ga:productName", "ga:productCategory", "ga:source"
      ),
      metrics = c("ga:itemQuantity", "ga:visits"), max.results = 10000, sort =
        "ga:date", table.id = "ga:34662915"
    )
  tf.spec.query <- QueryBuilder(tf.spec.init)
  print("TF-SPECIALS")
  tf.spec.dat <- try(GetReportData(tf.spec.query, rGA.token))
  tf.spec.dat$date <-
    paste(
      substr(tf.spec.dat$date, 1, 4) ,"-", substr(tf.spec.dat$date, 5, 6),"-", substr(tf.spec.dat$date, 7, 8), sep =
        ""
    )
  colnames(tf.spec.dat) <-
    c(
      "EVENT_TS", "CAMPAIGN", "PRODUCT", "PRODUCT_CATEGORY", "SOURCE", "QUANTITY", "VISITS"
    )
  write.csv(tf.spec.dat, file = "D://imm_automation/R_Files/TF-Specials.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/TF-Specials.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/TF-Specials.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TOTAL WIRELESS BRAND ----- 
  
  tw.brand.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:campaign", "ga:medium", "ga:source", "ga:adContent"
      ),
      metrics = c("ga:users"), max.results = 10000, sort =
        "ga:date", table.id = "ga:97642025"
    )
  tw.brand.query <- QueryBuilder(tw.brand.init)
  print("TW-BRAND")
  tw.brand.dat <- try(GetReportData(tw.brand.query, rGA.token))
  tw.brand.dat$date <-
    paste(
      substr(tw.brand.dat$date, 1, 4) ,"-", substr(tw.brand.dat$date, 5, 6),"-", substr(tw.brand.dat$date, 7, 8), sep =
        ""
    )
  colnames(tw.brand.dat) <-
    c("EVENT_TS", "CAMPAIGN", "MEDIUM", "SOURCE", "ADCONTENT", "USERS")
  write.csv(tw.brand.dat, file = "D://imm_automation/R_Files/TW-Brand.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/TW-Brand.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/TW-Brand.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TOTAL WIRELESS STORE -----   
  tw.store.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date"),
      metrics = c("ga:sessions", "ga:transactions", "ga:users"), max.results = 10000, sort =
        "ga:date", table.id = "ga:97642025"
    )
  tw.store.query <- QueryBuilder(tw.store.init)
  print("TW-STORE")
  tw.store.dat <- try(GetReportData(tw.store.query, rGA.token))
  tw.store.dat$date <-
    paste(
      substr(tw.store.dat$date, 1, 4) ,"-", substr(tw.store.dat$date, 5, 6),"-", substr(tw.store.dat$date, 7, 8), sep =
        ""
    )
  colnames(tw.store.dat) <-
    c("EVENT_TS", "SESSIONS", "TRANSACTIONS", "USERS")
  write.csv(tw.store.dat, file = "D://imm_automation/R_Files/TW-Store.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/TW-Store.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/TW-Store.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TOTAL WIRELESS SPECIALS -----   
  tw.spec.init <-
    Init(
      start.date, end.date, dimensions = c(
        "ga:date", "ga:adContent", "ga:campaign", "ga:medium", "ga:eventAction", "ga:eventCategory", "ga:source"
      ),
      metrics = c(
        "ga:sessions", "ga:transactionRevenue", "ga:transactions", "ga:visits"
      ), max.results = 10000, sort = "ga:date", table.id = "ga:97642025"
    )
  tw.spec.query <- QueryBuilder(tw.spec.init)
  print("TW-SPECIALS")
  tw.spec.dat <- try(GetReportData(tw.spec.query, rGA.token))
  tw.spec.dat$date <-
    paste(
      substr(tw.spec.dat$date, 1, 4) ,"-", substr(tw.spec.dat$date, 5, 6),"-", substr(tw.spec.dat$date, 7, 8), sep =
        ""
    )
  colnames(tw.spec.dat) <-
    c(
      "EVENT_TS", "ADCONTENT", "CAMPAIGN", "MEDIUM", "EVENT_ACTION", "EVENT_CATEGORY", "SOURCE", "SESSIONS", "REVENUE", "TRANSACTIONS", "VISITS"
    )
  write.csv(tw.spec.dat, file = "D://imm_automation/R_Files/TW-Specials.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/TW-Specials.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/daily_send_report/TW-Specials.csv", userpwd = "dgrabowski:dr0p123"
  )
  

  
  ################################################################################

  #ROKU QUERIES#
  
# ROKU SESSIONS ----- 
  roku.sessions.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date", "ga:adContent"),
      metrics = "ga:users, ga:bounces, ga:bounceRate, ga:sessions, ga:goal15Completions",
      max.results = 10000, sort = "ga:date", table.id =
        "ga:103898482"
    )
  roku.sessions.query <- QueryBuilder(roku.sessions.init)
  print("ROKU-SESSIONS")
  roku.sessions.dat <-
    GetReportData(roku.sessions.query, rGA.token, split_daywise = TRUE)
  roku.sessions.dat$date <-
    paste(
      substr(roku.sessions.dat$date, 1, 4) ,"-", substr(roku.sessions.dat$date, 5, 6),"-", substr(roku.sessions.dat$date, 7, 8), sep =
        ""
    )
  colnames(roku.sessions.dat) <-
    c(
      "EVENT_TS", "ADCONTENT", "USERS", "BOUNCES", "BOUNCERATE", "SESSIONS", "ACTIVATIONS"
    )
  roku.sessions.dat$ADCONTENT <-
    sub("'", "", roku.sessions.dat$ADCONTENT)
  roku.sessions.dat$BOUNCERATE <- round(roku.sessions.dat$BOUNCERATE, digits=2)
  
  write.csv(roku.sessions.dat, file = "D://imm_automation/R_Files/roku.sessions.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/roku.sessions.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku.sessions.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU ENGAGED -----   
  roku.engaged.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date", "ga:adContent"),
      metrics = "ga:users, ga:bounces, ga:bounceRate, ga:sessions, ga:goal15Completions",
      segments = "sessions::condition::ga:bounces==0;ga:sessionDuration>60", max.results = 10000, sort =
        "ga:date", table.id = "ga:103898482"
    )
  roku.engaged.query <- QueryBuilder(roku.engaged.init)
  print("ROKU-ENGAGED")
  roku.engaged.dat <-
    GetReportData(roku.engaged.query, rGA.token, split_daywise = TRUE)
  roku.engaged.dat$date <-
    paste(
      substr(roku.engaged.dat$date, 1, 4) ,"-", substr(roku.engaged.dat$date, 5, 6),"-", substr(roku.engaged.dat$date, 7, 8), sep =
        ""
    )
  colnames(roku.engaged.dat) <-
    c(
      "EVENT_TS", "ADCONTENT", "USERS", "BOUNCES", "BOUNCERATE", "SESSIONS", "ACTIVATIONS"
    )
  roku.engaged.dat$ADCONTENT <-
    sub("'", "", roku.engaged.dat$ADCONTENT)

  write.csv(roku.engaged.dat, file = "D://imm_automation/R_Files/roku.engaged.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/roku.engaged.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku.engaged.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU PROSPECTIVE -----  
  roku.prosp.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date", "ga:adContent"),
      metrics = "ga:sessions, ga:users,  ga:goal15Completions",
      segments = "users::condition::ga:sessions<=5;condition::ga:transactions==0;condition::ga:medium!~^EML;
      sequence::!^ga:landingPagePath=~welcome|link;sessions::condition::ga:pagePath!@/signin;ga:pagePath!@/about/employment;
      condition::!ga:eventAction=@Added Channel", max.results = 10000, sort =
        "ga:date", table.id = "ga:103898482"
    )
  roku.prosp.query <- QueryBuilder(roku.prosp.init)
  print("ROKU-PROSPECTIVE")
  roku.prosp.dat <-
    GetReportData(roku.prosp.query, rGA.token, split_daywise = TRUE)
  roku.prosp.dat$date <-
    paste(
      substr(roku.prosp.dat$date, 1, 4) ,"-", substr(roku.prosp.dat$date, 5, 6),"-", substr(roku.prosp.dat$date, 7, 8), sep =
        ""
    )
  colnames(roku.prosp.dat) <-
    c("EVENT_TS", "ADCONTENT", "SESSIONS", "USERS", "ACTIVATIONS")
  roku.prosp.dat$ADCONTENT <- sub("'", "", roku.prosp.dat$ADCONTENT)

  write.csv(roku.prosp.dat, file = "D://imm_automation/R_Files/roku.prospective.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/roku.prospective.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku.prospective.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU SITE INFLUENCED ACTIVIATIONS -----  
  roku.inf.init <-
    Init(
      start.date, end.date, dimensions = c("ga:date", "ga:adContent"),
      metrics = "ga:sessions, ga:users,  ga:goal15Completions",
      segments = "users::sequence::ga:sessionCount==1;ga:pagePath!~my.roku.com/welcome*;->>ga:pagePath=~my.roku.com/welcome*",
      max.results = 10000, sort = "ga:date", table.id =
        "ga:103898482"
    )
  roku.inf.query <- QueryBuilder(roku.inf.init)
  print("ROKU-SITE.INFLUENCED")
  roku.inf.dat <-
    GetReportData(roku.inf.query, rGA.token, split_daywise = TRUE)
  roku.inf.dat$date <-
    paste(
      substr(roku.inf.dat$date, 1, 4) ,"-", substr(roku.inf.dat$date, 5, 6),"-", substr(roku.inf.dat$date, 7, 8), sep =
        ""
    )
  colnames(roku.inf.dat) <-
    c("EVENT_TS", "ADCONTENT", "SESSIONS", "USERS", "ACTIVATIONS")
  roku.inf.dat$ADCONTENT <- sub("'", "", roku.inf.dat$ADCONTENT)

  write.csv(roku.inf.dat, file = "D://imm_automation/R_Files/roku.SiteInfluencedActivations.csv", row.names = FALSE)
  ftpUpload(
    "D://imm_automation/R_Files/roku.SiteInfluencedActivations.csv",
    "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku.SiteInfluencedActivations.csv", userpwd = "dgrabowski:dr0p123"
  )
  

# NETEZZA DATA LOAD CALL -----    
  print("Running Google Analytics Netezza Load Script")
  
  call <-
    paste("call LOAD_GOOGLE_ANALYTICS_DATA('", start.date, "','", end.date, "', 1)", sep =
            "")
  nzQuery(call)
  
  nzDisconnect()
  
  print("Google Analytics Netezza Load Script Complete")
  
  #####
}
