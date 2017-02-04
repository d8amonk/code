library(RGoogleAnalytics)
ga.alex.sandbox <- Auth("582273035648-anfjsicqiusq058ctsup364j8q2teegt.apps.googleusercontent.com","6jWM5YG6c3w-Sn-DqsRMbD2F")
save(ga.alex.sandbox,file="ga.alex.sandbox")
GetProfiles(ga.alex.sandbox)

query.list <- Init(start.date = "2014-01-01",
                   end.date = "2015-09-04",
                   dimensions = "ga:date",
                   metrics = "ga:sessions,ga:pageviews",
                   max.results = 1000,
                   table.id = "ga:94580156")
ga.query <- QueryBuilder(query.list)
ga.df <- GetReportData(ga.query, ga.alex.sandbox)
ga.df(ga.query, ga.alex.sandbox)

library(lubridate)
ga.df$date <- ymd(ga.df$date)
plot(ga.df)

#can we pull from GA and dump into NZ without housing the data?

g <- ggplot(ga.df, aes(x = date, y=sessions))
g+geom_line()+theme_economist()+ggtitle("Sessions Over Time! Wuuuut!")
