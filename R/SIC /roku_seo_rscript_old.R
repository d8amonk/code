#R Script for Automating Search Console Data#


rseo.pull <- function(start.date = as.character(today() - 10)) {
 
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
  
  
# ROKU FULL SITE SEO QUERIES - https://www.roku.com/ ----------------
  
  roku.seo.full <-
    search_analytics(
      "https://www.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full)[1] <- "EVENT_TS"
  
  
  roku.seo.page <-
    search_analytics(
      "https://www.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page)[1] <- "EVENT_TS"
  
  
  roku.seo.query <-
    search_analytics(
      "https://www.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query)[1] <- "EVENT_TS"
  
  
  roku.seo.full <-
    cbind(SITE = "Main", roku.seo.full)
  roku.seo.full <- roku.seo.full[,c(2,1,3:11)]

  roku.seo.page <-
    cbind(SITE = "Main", roku.seo.page)
  roku.seo.page <- roku.seo.page[,c(2,1,3:10)]
  
  roku.seo.query <-
    cbind(SITE = "Main", roku.seo.query)
  roku.seo.query <- roku.seo.query[,c(2,1,3:10)]
  
  
  full.file <-
    "D://imm_automation/R_Files/full_main.csv"
  page.file <-
    "D://imm_automation/R_Files/page_main.csv"
  query.file <-
    "D://imm_automation/R_Files/query_main.csv"
  
  
  write.csv(roku.seo.full, file = full.file, row.names = F)
  write.csv(roku.seo.page, file = page.file, row.names = F)
  write.csv(roku.seo.query, file = query.file, row.names = F)
  
  
  ftpUpload(
    full.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU SUPPORT SECURED SITE SEO QUERIES - https://support.roku.com/ ----------------
  
  roku.seo.full.supsec <-
    search_analytics(
      "https://support.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full.supsec)[1] <- "EVENT_TS"
  
  
  roku.seo.page.supsec <-
    search_analytics(
      "https://support.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page.supsec)[1] <- "EVENT_TS"
  
  
  roku.seo.query.supsec <-
    search_analytics(
      "https://support.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query.supsec)[1] <- "EVENT_TS"
  
  
  roku.seo.full.supsec <-
    cbind(SITE = "Support Secured", roku.seo.full.supsec)
  roku.seo.full.supsec <- roku.seo.full.supsec[,c(2,1,3:11)]
  
  roku.seo.page.supsec <-
    cbind(SITE = "Support Secured", roku.seo.page.supsec)
  roku.seo.page.supsec <- roku.seo.page.supsec[,c(2,1,3:10)]

  roku.seo.query.supsec <-
    cbind(SITE = "Support Secured", roku.seo.query.supsec)
  roku.seo.query.supsec <- roku.seo.query.supsec[,c(2,1,3:10)]

  
  
  full.supsec.file <-
    "D://imm_automation/R_Files/full_supsec.csv"
  page.supsec.file <-
    "D://imm_automation/R_Files/page_supsec.csv"
  query.supsec.file <-
    "D://imm_automation/R_Files/query_supsec.csv"
  
  
  write.csv(roku.seo.full.supsec, file = full.supsec.file, row.names = F)
  write.csv(roku.seo.page.supsec, file = page.supsec.file, row.names = F)
  write.csv(roku.seo.query.supsec, file = query.supsec.file, row.names = F)
  
  
  ftpUpload(
    full.supsec.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_supsec.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.supsec.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_supsec.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.supsec.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_supsec.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU SUPPORT NON-SECURED SITE SEO QUERIES - http://support.roku.com/ ----------------
  
  roku.seo.full.supnon <-
    search_analytics(
      "http://support.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full.supnon)[1] <- "EVENT_TS"
  
  
  roku.seo.page.supnon <-
    search_analytics(
      "http://support.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page.supnon)[1] <- "EVENT_TS"
  
  
  roku.seo.query.supnon <-
    search_analytics(
      "http://support.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query.supnon)[1] <- "EVENT_TS"
  
  
  roku.seo.full.supnon <-
    cbind(SITE = "Support Non-Secured", roku.seo.full.supnon)
  roku.seo.full.supnon <- roku.seo.full.supnon[,c(2,1,3:11)]

  roku.seo.page.supnon <-
    cbind(SITE = "Support Non-Secured", roku.seo.page.supnon)
  roku.seo.page.supnon <- roku.seo.page.supnon[,c(2,1,3:10)]

  roku.seo.query.supnon <-
    cbind(SITE = "Support Non-Secured", roku.seo.query.supnon)
  roku.seo.query.supnon <- roku.seo.query.supnon[,c(2,1,3:10)]

  
  
  full.supnon.file <-
    "D://imm_automation/R_Files/full_supnon.csv"
  page.supnon.file <-
    "D://imm_automation/R_Files/page_supnon.csv"
  query.supnon.file <-
    "D://imm_automation/R_Files/query_supnon.csv"
  
  
  write.csv(roku.seo.full.supnon, file = full.supnon.file, row.names = F)
  write.csv(roku.seo.page.supnon, file = page.supnon.file, row.names = F)
  write.csv(roku.seo.query.supnon, file = query.supnon.file, row.names = F)
  
  
  ftpUpload(
    full.supnon.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_supnon.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.supnon.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_supnon.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.supnon.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_supnon.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU MY SITE SEO QUERIES - https://my.roku.com/ ----------------
  
  roku.seo.full.my <-
    search_analytics(
      "https://my.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full.my)[1] <- "EVENT_TS"
  
  
  roku.seo.page.my <-
    search_analytics(
      "https://my.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page.my)[1] <- "EVENT_TS"
  
  
  roku.seo.query.my <-
    search_analytics(
      "https://my.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query.my)[1] <- "EVENT_TS"
  
  
  roku.seo.full.my <-
    cbind(SITE = "My", roku.seo.full.my)
  roku.seo.full.my <- roku.seo.full.my[,c(2,1,3:11)]

  roku.seo.page.my <-
    cbind(SITE = "My", roku.seo.page.my)
  roku.seo.page.my <- roku.seo.page.my[,c(2,1,3:10)]

  roku.seo.query.my <-
    cbind(SITE = "My", roku.seo.query.my)
  roku.seo.query.my <- roku.seo.query.my[,c(2,1,3:10)]

  
  
  full.my.file <-
    "D://imm_automation/R_Files/full_my.csv"
  page.my.file <-
    "D://imm_automation/R_Files/page_my.csv"
  query.my.file <-
    "D://imm_automation/R_Files/query_my.csv"
  
  
  write.csv(roku.seo.full.my, file = full.my.file, row.names = F)
  write.csv(roku.seo.page.my, file = page.my.file, row.names = F)
  write.csv(roku.seo.query.my, file = query.my.file, row.names = F)
  
  
  ftpUpload(
    full.my.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_my.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.my.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_my.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.my.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_my.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU FORUMS SITE SEO QUERIES - http://forums.roku.com/ ----------------
  
  roku.seo.full.forum <-
    search_analytics(
      "http://forums.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full.forum)[1] <- "EVENT_TS"
  
  
  roku.seo.page.forum <-
    search_analytics(
      "http://forums.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page.forum)[1] <- "EVENT_TS"
  
  
  roku.seo.query.forum <-
    search_analytics(
      "http://forums.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query.forum)[1] <- "EVENT_TS"
  
  
  roku.seo.full.forum <-
    cbind(SITE = "Forum", roku.seo.full.forum)
  roku.seo.full.forum <- roku.seo.full.forum[,c(2,1,3:11)]

  roku.seo.page.forum <-
    cbind(SITE = "Forum", roku.seo.page.forum)
  roku.seo.page.forum <- roku.seo.page.forum[,c(2,1,3:10)]

  roku.seo.query.forum <-
    cbind(SITE = "Forum", roku.seo.query.forum)
  roku.seo.query.forum <- roku.seo.query.forum[,c(2,1,3:10)]

  
  
  full.forum.file <-
    "D://imm_automation/R_Files/full_forum.csv"
  page.forum.file <-
    "D://imm_automation/R_Files/page_forum.csv"
  query.forum.file <-
    "D://imm_automation/R_Files/query_forum.csv"
  
  
  write.csv(roku.seo.full.forum, file = full.forum.file, row.names = F)
  write.csv(roku.seo.page.forum, file = page.forum.file, row.names = F)
  write.csv(roku.seo.query.forum, file = query.forum.file, row.names = F)
  
  
  ftpUpload(
    full.forum.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_forum.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.forum.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_forum.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.forum.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_forum.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU BLOG SITE SEO QUERIES - https://blog.roku.com/ ----------------
  
  roku.seo.full.blog <-
    search_analytics(
      "https://blog.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full.blog)[1] <- "EVENT_TS"
  
  
  roku.seo.page.blog <-
    search_analytics(
      "https://blog.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page.blog)[1] <- "EVENT_TS"
  
  
  roku.seo.query.blog <-
    search_analytics(
      "https://blog.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query.blog)[1] <- "EVENT_TS"
  
  
  roku.seo.full.blog <-
    cbind(SITE = "Blog", roku.seo.full.blog)
  roku.seo.full.blog <- roku.seo.full.blog[,c(2,1,3:11)]

  roku.seo.page.blog <-
    cbind(SITE = "Blog", roku.seo.page.blog)
  roku.seo.page.blog <- roku.seo.page.blog[,c(2,1,3:10)]

  roku.seo.query.blog <-
    cbind(SITE = "Blog", roku.seo.query.blog)
  roku.seo.query.blog <- roku.seo.query.blog[,c(2,1,3:10)]

  
  
  full.blog.file <-
    "D://imm_automation/R_Files/full_blog.csv"
  page.blog.file <-
    "D://imm_automation/R_Files/page_blog.csv"
  query.blog.file <-
    "D://imm_automation/R_Files/query_blog.csv"
  
  
  write.csv(roku.seo.full.blog, file = full.blog.file, row.names = F)
  write.csv(roku.seo.page.blog, file = page.blog.file, row.names = F)
  write.csv(roku.seo.query.blog, file = query.blog.file, row.names = F)
  
  
  ftpUpload(
    full.blog.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_blog.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.blog.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_blog.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.blog.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_blog.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# ROKU CHANNEL STORE SITE SEO QUERIES - https://channelstore.roku.com/ ----------------
  
  roku.seo.full.channel <-
    search_analytics(
      "https://channelstore.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.full.channel)[1] <- "EVENT_TS"
  
  
  roku.seo.page.channel <-
    search_analytics(
      "https://channelstore.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.page.channel)[1] <- "EVENT_TS"
  
  
  roku.seo.query.channel <-
    search_analytics(
      "https://channelstore.roku.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(roku.seo.query.channel)[1] <- "EVENT_TS"
  
  
  roku.seo.full.channel <-
    cbind(SITE = "Channel", roku.seo.full.channel)
  roku.seo.full.channel <- roku.seo.full.channel[,c(2,1,3:11)]

  roku.seo.page.channel <-
    cbind(SITE = "Channel", roku.seo.page.channel)
  roku.seo.page.channel <- roku.seo.page.channel[,c(2,1,3:10)]

  roku.seo.query.channel <-
    cbind(SITE = "Channel", roku.seo.query.channel)
  roku.seo.query.channel <- roku.seo.query.channel[,c(2,1,3:10)]

  
  
  full.channel.file <-
    "D://imm_automation/R_Files/full_channel.csv"
  page.channel.file <-
    "D://imm_automation/R_Files/page_channel.csv"
  query.channel.file <-
    "D://imm_automation/R_Files/query_channel.csv"
  
  
  write.csv(roku.seo.full.channel, file = full.channel.file, row.names = F)
  write.csv(roku.seo.page.channel, file = page.channel.file, row.names = F)
  write.csv(roku.seo.query.channel, file = query.channel.file, row.names = F)
  
  
  ftpUpload(
    full.channel.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_full_channel.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.channel.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_page_channel.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.channel.file, "sftp://infosphere.imm.com/archive/dropoff/client_roku/roku_seo_query_channel.csv", userpwd = "dgrabowski:dr0p123"
  )
  
  ######################################################
  
# STRAIGHTALK BRAND SEO QUERIES  - https://www.straighttalk.com/ ----------------
  st.seo.full.main <-
    search_analytics(
      "https://www.straighttalk.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(st.seo.full.main)[1] <- "EVENT_TS"
  
  
  st.seo.page.main <-
    search_analytics(
      "https://www.straighttalk.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(st.seo.page.main)[1] <- "EVENT_TS"
  
  
  st.seo.query.main <-
    search_analytics(
      "https://www.straighttalk.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(st.seo.query.main)[1] <- "EVENT_TS"
  
  
  st.seo.full.main <-
    cbind(SITE = "Main", st.seo.full.main)
  st.seo.full.main <- st.seo.full.main[,c(2,1,3:11)]
  
  st.seo.page.main <-
    cbind(SITE = "Main", st.seo.page.main)
  st.seo.page.main <- st.seo.page.main[,c(2,1,3:10)]
  
  st.seo.query.main <-
    cbind(SITE = "Main", st.seo.query.main)
  st.seo.query.main <- st.seo.query.main[,c(2,1,3:10)]
  
  
  
  full.st.main.file <-
    "D://imm_automation/R_Files/full_st_main.csv"
  page.st.main.file <-
    "D://imm_automation/R_Files/page_st_main.csv"
  query.st.main.file <-
    "D://imm_automation/R_Files/query_st_main.csv"
  
  
  write.csv(st.seo.full.main, file = full.st.main.file, row.names = F)
  write.csv(st.seo.page.main, file = page.st.main.file, row.names = F)
  write.csv(st.seo.query.main, file = query.st.main.file, row.names = F)
  
  
  ftpUpload(
    full.st.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/st_seo_full_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.st.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/st_seo_page_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.st.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/st_seo_query_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  
# TRACFONE BRAND SEO QUERIES  - http://www.tracfone.com/ ----------------
  tf.seo.full.main <-
    search_analytics(
      "http://www.tracfone.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(tf.seo.full.main)[1] <- "EVENT_TS"
  
  
  tf.seo.page.main <-
    search_analytics(
      "http://www.tracfone.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(tf.seo.page.main)[1] <- "EVENT_TS"
  
  
  tf.seo.query.main <-
    search_analytics(
      "http://www.tracfone.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(tf.seo.query.main)[1] <- "EVENT_TS"
  
  
  tf.seo.full.main <-
    cbind(SITE = "Main", tf.seo.full.main)
  tf.seo.full.main <- tf.seo.full.main[,c(2,1,3:11)]
  
  tf.seo.page.main <-
    cbind(SITE = "Main", tf.seo.page.main)
  tf.seo.page.main <- tf.seo.page.main[,c(2,1,3:10)]
  
  tf.seo.query.main <-
    cbind(SITE = "Main", tf.seo.query.main)
  tf.seo.query.main <- tf.seo.query.main[,c(2,1,3:10)]
  
  
  
  full.tf.main.file <-
    "D://imm_automation/R_Files/full_tf_main.csv"
  page.tf.main.file <-
    "D://imm_automation/R_Files/page_tf_main.csv"
  query.tf.main.file <-
    "D://imm_automation/R_Files/query_tf_main.csv"
  
  
  write.csv(tf.seo.full.main, file = full.tf.main.file, row.names = F)
  write.csv(tf.seo.page.main, file = page.tf.main.file, row.names = F)
  write.csv(tf.seo.query.main, file = query.tf.main.file, row.names = F)
  
  
  ftpUpload(
    full.tf.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/tf_seo_full_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.tf.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/tf_seo_page_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.tf.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/tf_seo_query_main.csv", userpwd = "dgrabowski:dr0p123"
  )  
  
# TOTAL WIRELESS BRAND SEO QUERIES  - https://www.totalwireless.com/ ----------------
  tw.seo.full.main <-
    search_analytics(
      "https://www.totalwireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(tw.seo.full.main)[1] <- "EVENT_TS"
  
  
  tw.seo.page.main <-
    search_analytics(
      "https://www.totalwireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(tw.seo.page.main)[1] <- "EVENT_TS"
  
  
  tw.seo.query.main <-
    search_analytics(
      "https://www.totalwireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(tw.seo.query.main)[1] <- "EVENT_TS"
  
  
  tw.seo.full.main <-
    cbind(SITE = "Main", tw.seo.full.main)
  tw.seo.full.main <- tw.seo.full.main[,c(2,1,3:11)]
  
  tw.seo.page.main <-
    cbind(SITE = "Main", tw.seo.page.main)
  tw.seo.page.main <- tw.seo.page.main[,c(2,1,3:10)]
  
  tw.seo.query.main <-
    cbind(SITE = "Main", tw.seo.query.main)
  tw.seo.query.main <- tw.seo.query.main[,c(2,1,3:10)]
  
  
  
  full.tw.main.file <-
    "D://imm_automation/R_Files/full_tw_main.csv"
  page.tw.main.file <-
    "D://imm_automation/R_Files/page_tw_main.csv"
  query.tw.main.file <-
    "D://imm_automation/R_Files/query_tw_main.csv"
  
  
  write.csv(tw.seo.full.main, file = full.tw.main.file, row.names = F)
  write.csv(tw.seo.page.main, file = page.tw.main.file, row.names = F)
  write.csv(tw.seo.query.main, file = query.tw.main.file, row.names = F)
  
  
  ftpUpload(
    full.tw.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/tw_seo_full_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.tw.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/tw_seo_page_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.tw.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/tw_seo_query_main.csv", userpwd = "dgrabowski:dr0p123"
  ) 
  
# NET10 BRAND SEO QUERIES  - http://www.net10wireless.com/ + http://www.net10.com/ ----------------
  nt.wireless.seo.full.main <-
    search_analytics(
      "http://www.net10wireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(nt.wireless.seo.full.main)[1] <- "EVENT_TS"
  
  
  nt.wireless.seo.page.main <-
    search_analytics(
      "http://www.net10wireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(nt.wireless.seo.page.main)[1] <- "EVENT_TS"
  
  
  nt.wireless.seo.query.main <-
    search_analytics(
      "http://www.net10wireless.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(nt.wireless.seo.query.main)[1] <- "EVENT_TS"
  
  
  nt.wireless.seo.full.main <-
    cbind(SITE = "Main", nt.wireless.seo.full.main)
  nt.wireless.seo.full.main <- nt.wireless.seo.full.main[,c(2,1,3:11)]
  
  nt.wireless.seo.page.main <-
    cbind(SITE = "Main", nt.wireless.seo.page.main)
  nt.wireless.seo.page.main <- nt.wireless.seo.page.main[,c(2,1,3:10)]
  
  nt.wireless.seo.query.main <-
    cbind(SITE = "Main", nt.wireless.seo.query.main)
  nt.wireless.seo.query.main <- nt.wireless.seo.query.main[,c(2,1,3:10)]
  
  
  nt.com.seo.full.main <-
    search_analytics(
      "http://www.net10.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(nt.com.seo.full.main)[1] <- "EVENT_TS"
  
  
  nt.com.seo.page.main <-
    search_analytics(
      "http://www.net10.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'page'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(nt.com.seo.page.main)[1] <- "EVENT_TS"
  
  
  nt.com.seo.query.main <-
    search_analytics(
      "http://www.net10.com/", start.date, end.date, dimensions = c('date', 'country', 'device', 'query'),
      searchType = 'web', aggregationType = 'auto', rowLimit = 5000, prettyNames =
        F, dimensionFilterExp = "country==USA"
    )
  colnames(nt.com.seo.query.main)[1] <- "EVENT_TS"
  
  
  nt.com.seo.full.main <-
    cbind(SITE = "Main", nt.com.seo.full.main)
  nt.com.seo.full.main <- nt.com.seo.full.main[,c(2,1,3:11)]
  
  nt.com.seo.page.main <-
    cbind(SITE = "Main", nt.com.seo.page.main)
  nt.com.seo.page.main <- nt.com.seo.page.main[,c(2,1,3:10)]
  
  nt.com.seo.query.main <-
    cbind(SITE = "Main", nt.com.seo.query.main)
  nt.com.seo.query.main <- nt.com.seo.query.main[,c(2,1,3:10)]
  
  
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
  
  
  ftpUpload(
    full.nt.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/nt_seo_full_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    page.nt.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/nt_seo_page_main.csv", userpwd = "dgrabowski:dr0p123"
  )
  ftpUpload(
    query.nt.main.file, "sftp://infosphere.imm.com/archive/dropoff/client_tracfone/nt_seo_query_main.csv", userpwd = "dgrabowski:dr0p123"
  )  
  
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