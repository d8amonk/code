sample_size <- function(variations = 1, conv_rate = 0.015, performance = 0.12, traffic = 10000){
  unique_views = 16*(variations+1)*((sqrt(conv_rate*(1-conv_rate))/(conv_rate*performance))^2)
  print(paste("You need to run this test for ", round(unique_views/traffic), 
              " days, given a sample size of ", round(unique_views), sep = ""))
}
# sample_size()
# "You need to run this test for 15 days, given a sample size of 145926"