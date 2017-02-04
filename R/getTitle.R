getTitle <- function(url) {
  webpage <- readLines(url)
  first.row <- webpage[1]
  start <- regexpr("<title>", first.row)
  end <- regexpr("</title>", first.row)
  title <- substr(first.row,start + 7,end - 1)
  return(title)
}

getTitles <- function(pages) {
  my.matrix <- matrix(NA, ncol = 2, nrow = nrow(pages))
  for (i in seq_along(1:nrow(pages))) {
    my.matrix[i,1] <- as.character(pages[i,])
    my.matrix[i,2] <- getTitle(as.character(pages[i,])) }
  return(my.matrix)
  print(my.matrix)}
#
# url    <- as.character(mypages)
# title  <- sapply(url, getTitle)
# report <- data.frame(url, title)
# write.csv(report, file = "report.csv", row.names = FALSE)