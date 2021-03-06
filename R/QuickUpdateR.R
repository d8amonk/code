# To reinstall packages from one R version to the next: 
#Currently you can do 

tmp <- installed.packages() 
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1]) 
save(installedpkgs, file="c:/Users/jvangeete/Desktop/installed.rda") 

#in the old version to get a list of packages you installed.  Then in the 
#new version, 

load("c:/R/installed.rda") 
install.packages(installedpkgs) 

# OR if it's too late for all that...
my.pkgs <- c('nzr',
             'searchConsoleR',
             'nza',
             'RGA',
             'googleAuthR',
             'RGoogleAnalytics',
             'lubridate', 
             'scales',
             'ggplot2',
             'ggthemes',
             'plyr',
             'dplyr',
             'stargazer',
             'installr', 
             'fpp',
             'forecast',
             'xtable',
             'plotly',
             'RODBC',
             'RMySQL',
             'RPostgresSQL',
             'RSQLite',
             'stargazer',
             'XLConnect',
             'xlsx',
             'foreign',
             'tidyr',
             'stringr',
             'ggvis',
             'rgl',
             'htmlwidgets',
             'leaflet',
             'dygraphs',
             'DT',
             'diagrammeR',
             'network3D',
             'threeJS',
             'googleVis',
             'car',
             'mgcv',
             'lme4',
             'nlme',
             'randomForest',
             'multcomp',
             'vcd',
             'glmnet',
             'survival',
             'caret',
             'shiny',
             'Rmarkdown',
             'sp',
             'maptools',
             'maps',
             'ggmap',
             'zoo',
             'xts',
             'quantmod',
             'rcpp',
             'data.table',
             'parallel',
             'XML',
             'jsonlite',
             'httr',
             'devtools',
             'testthat',
             'roxygen2',
             "Hmisc",
             "CausalImpact",
             quietly = T,
             warn.conflicts = F)  ## and so on for my preferred packages 
install.packages(my.pkgs, dependencies=TRUE) 
