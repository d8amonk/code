# load package 'gdata'
library(gdata)

# excel file (1st worksheet named "dades")
alpha_xls = "http://www.lsi.upc.edu/~belanche/Docencia/mineria/Practiques/alpha.xls"

# how many sheets
sheetCount(alpha_xls)

sheetNames(alpha_xls)

# import sheet 1 (dades) in R
alpha_data = read.xls(alpha_xls, sheet = 1)
head(alpha_data)