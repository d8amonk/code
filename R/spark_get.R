require(sparklyr)
require(rJava)
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_111\\jre')
devtools::install_github("rstudio/sparklyr")

# if (!require('devtools')) install.packages('devtools')
# devtools::install_github('apache/spark@v1.4.0', subdir='R/pkg')
# jsonlite::fromJSON("https://api.github.com/repos/apache/spark/tags")$name
Sys.setenv(SPARK_HOME = "/Users/jvangeete/spark-2.0.2-bin-hadoop2.7/")
Sys.setenv(JAVA_HOME = "c:/Program Files (x86)/Java/jre1.8.0_101/bin/")
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)

options("sparkapi.ports.file" = "ports.out")



sc <- spark_connect(master = "local", config = list())#,sparkEnvir = list(spark.driver.memory="2g"))

sqlContext <- sparkRSQL.s(sc)
df <- createDataFrame(sqlContext, iris)
head(df)


spark_install(version = "1.6.2")




# sc <- spark_connect("yarn-client")
