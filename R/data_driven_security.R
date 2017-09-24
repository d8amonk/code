# ch3
setwd('/Users/jvg/git/code/data_driven_security/book/ch03/')
pkgs <- c(
  'ggplot2',
  'scales',
  'maptools',
  'sp',
  'maps',
  'grid',
  'car'
)

new.pkgs <- pkgs[!(pkgs %in% installed.packages())]
if (length(new.pkgs)) {
  install.packages(new.pkgs)
}

avURL <- 'http://datadrivensecurity.info/book/ch03/data/reputation.data'
avRep <- 'data/reputation.data'

if (file.access(avRep)) {
  download.file(avURL, avRep)
}
open('data/reputation.data')

av <- read.csv(
  avRep,
  sep = '#',
  header = F
)

colnames(av) <- c(
  'IP',
  'Reliability',
  'Risk',
  'Type',
  'Country',
  'Locale',
  'Coords',
  'x'
)

str(av)
head(av)
summary(av)
d3heatmap::d3heatmap(
  table(av$Country, av$Risk)
)
