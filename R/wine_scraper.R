library(dplyr)
library(rvest)

site = read_html("http://www.winemag.com/?s=stellenbosch&drink_type=wine&page=1")
nr_pages <- html_nodes(site,".pagination li a") %>%
  html_text() %>% tail(.,1) %>% as.numeric

wine_href <- html_nodes(site,".review-listing") %>% html_attr("href")

info_xtr <- function(x){
  page <- read_html(x)
  review <- page %>% html_nodes("p.description") %>% 
    html_text
  review_info <- page %>% html_nodes(".info.small-9.columns span span") %>% 
    html_text()
  cbind(review, date = review_info[5], Alc = review_info[1])
}

info_xtr(wine_href[1])

# Start the scraper ==============
Wine_cellar <- list()
for(i in 1:nr_pages)
{
  
  cat("Wine review, page: ", i,"\n")
  
  site = read_html(paste0("http://www.winemag.com/?s=stellenbosch&drink_type=wine&page=",i))
  
  wine <- html_nodes(site,".review-listing .title") %>%
    html_text()
  
  # extract specific information from title
  wine_farms <- gsub(" \\d.*$","",wine)
  wine_year <- gsub("[^0-9]","",wine)
  wine_name <- gsub("^.*\\d ","",wine) %>% gsub(" \\(Stellenbosch\\)","",.)
  
  # extract review points
  points <- html_nodes(site,".info .rating") %>%
    html_text() %>% gsub(" Points","",.)
  
  # extract review price
  price <- html_nodes(site,"span.price") %>%
    html_text()
  
  wine_href <- html_nodes(site,".review-listing") %>% html_attr("href")
  
  # here I go collect all the information from each of the wines seperately
  reviews <- sapply(wine_href, function(x) info_xtr(x))
  reviews <- t(reviews) 
  row.names(reviews) <- NULL
  colnames(reviews) <- c("review", "review_date", "Alc")
  
  # bind all the information into a nicely formatted data.frame()
  Wine_cellar[[i]] <- data.frame(wine_farms, wine_year, wine_name, points, price, wine_href, reviews, stringsAsFactors = F)
  
  # as a rule I always save the already collected data, just in case the scraper stops unexpectedly
  saveRDS(Wine_cellar,"Wine_collection.RDS")
  
  # I add in a sleep as not the flood the website that I am scraping with requests
  Sys.sleep(runif(1,0,3))
}