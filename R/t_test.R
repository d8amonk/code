library(foreign)

file.choose()
# "C:\\Users\\jvangeete\\Google Drive\\School\\UCD\\AppStat\\hw01surgical.sav"
s <- read.spss("C:\\Users\\jvangeete\\Google Drive\\School\\UCD\\AppStat\\hw01surgical.sav", to.data.frame = TRUE)

t.test(s, conf.level = .9, mu = 98.2)


critical.t <- function(){
  cat("\n","\bEnter Alpha Level","\n")
  alpha<-scan(n=1,what = double(0),quiet=T)
  cat("\n","\b1 Tailed or 2 Tailed:\nEnter either 1 or 2","\n")
  tt<-scan(n=1,what = double(0),quiet=T)
  cat("\n","\bEnter Number of Observations","\n")
  n<-scan(n=1,what = double(0),quiet=T)
  cat("\n\nCritical Value =",qt(1-(alpha/tt), n-2), "\n")
}