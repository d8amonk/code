site <- "https://www.gutenberg.org/cache/epub/2701/pg2701.txt"
moby_url <- url(site) #Moby Dick

moby_dick <- readLines(moby_url, n = )

# remember to play fair
download.file(site, "mobydick.txt")
close(moby_url)


moby_dick_chap1 <- rep("", 10)
skip = 535

# read the first 10 lines of Moby Dick, Chapter 1
for (i in 1L:10){
  one_line = scan("mobydick.txt", what = "", skip = skip, nlines = 1)
  moby_dick_chap1[i] <- paste(one_line, collapse = " ")
  skip <- skip + 1
}

# lines 536-545
moby_dick_chap1
