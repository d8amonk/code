library(stringr) # used for str_extract()
library(ggplot2) # used for plotting
library(plyr) # used for count()

#########################
#### Scrape function ####
#########################
scrape.pubmed <- function(x,y) {
  results <- pubmed_result[5]
  results <- results[results != "ShortDetails"]
  year <- str_extract(results, "(19[0-9]{2}|20[0-1][0-9])")
  count <- count(year[year < 2012])
  count$cat <- x
  count$type <- y
  return(count)
}

########################
#### scrape pubmed #####
########################

# read.delim is used on some files because the csv wouldn't be read
# correctly. Excel was used to save the csv-files as tab delim. files.

# borderline
pubmed_result <- read.delim("~/Downloads/pubmed_borderline.txt")
data.long <- scrape.pubmed("borderline","personality disorder")

# ADHD
pubmed_result <- read.csv("~/Downloads/pubmed_ADHD.csv", row.names = NULL)
data.long <- rbind(data.long,
                   scrape.pubmed("ADHD","Attention deficit ... behavior disorder"))

# Depression
pubmed_result <- read.delim("~/Downloads/pubmed_depression.txt")
data.long <- rbind(data.long, scrape.pubmed("depression","mood disorder"))

# Bulimia
pubmed_result <- read.csv("~/Downloads/pubmed_bulimia.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("bulimia","eating disorder"))

# Anorexia
pubmed_result <- read.csv("~/Downloads/pubmed_anorexia.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("anorexia", "eating disorder"))

# GAD
pubmed_result <- read.csv("~/Downloads/pubmed_gad.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("GAD", "anxiety disorder"))

# PTSD
pubmed_result <- read.csv("~/Downloads/pubmed_ptsd.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("PTSD", "anxiety disorder"))

# OCD
pubmed_result <- read.csv("~/Downloads/pubmed_ocd.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("OCD", "anxiety disorder"))

# Specific phobia
pubmed_result <- read.csv("~/Downloads/pubmed_specific_phobia.csv",
                          row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed(
  "Specific phobia", "anxiety disorder"))

# Agoraphobia
pubmed_result <- read.csv("~/Downloads/pubmed_agoraphobia.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("agoraphobia", "anxiety disorder"))

# Panic disorder
pubmed_result <- read.csv("~/Downloads/pubmed_panic.csv", row.names = NULL)
data.long <- rbind(data.long, scrape.pubmed("Panic disorder", "anxiety disorder"))

# Bipolar disorder
pubmed_result <- read.delim("~/Downloads/pubmed_bipolar.txt")
data.long <- rbind(data.long, scrape.pubmed("Bipolar disorder", "mood disorder"))

# Social phobia
pubmed_result <- read.csv("~/Downloads/pubmed_socialphobia.csv", row.names = NULL)
data.long<- rbind(data.long, scrape.pubmed("social phobia", "anxiety disorder"))

###############
#### plot #####
###############

# Used for a plot without depression.
data.long <- data.long[data.long$cat != "depression",]

# Custom linetypes
line.type <- c("anxiety disorder" = "solid",
               "mood disorder" = "longdash",
               "eating disorder" = "dashed",
               "personality disorder" = "twodash",
               "Attention deficit ... behavior disorder" = "dotted"
)

# Ggplot syntax, saved in 'p' for use with direct.label()
p <- ggplot(data.long, aes(x, freq, group = cat, color = cat, linetype=type)) +
  geom_line() +
  scale_x_discrete(breaks = seq(1900,2011,5),
                   limits=as.character(seq(1970,2021))) +
  opts(title="Number of PubMed hits per year and DSM-IV diagnosis",
       axis.text.x=theme_text(angle=45)) +
  xlab("Publication year") +
  ylab("Hits") +
  scale_linetype_manual("Disorder type", values=line.type)

# Use direct.label with the saved ggplot-syntax, to get direct labels after
# each line.
direct.label(p, "last.bumpup")