# The emails vector has already been defined for you
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org", 
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

# Use grep() to match for .edu addresses more robustly
grep("@.*\\.edu$", emails)

# Use grepl() to match for .edu addresses more robustly, save result to hits
hits <- grepl("@.*\\.edu$", emails)

# Subset emails using hits
emails[hits]