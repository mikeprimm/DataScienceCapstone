suppressPackageStartupMessages(c(
  library(shiny),
  library(RSQLite),
  require(NLP),
  require(openNLP),
  require(tm),
  require(RWeka),
  require(R.utils),
  require(SnowballC),
  require(wordcloud),
  require(pryr)
))

if (!file.exists("freqTable.db")) {
  if (!file.exists("freqTable.db.gz")) {
    download.file("http://mikeprimm.com/DataScienceCapstone/freqTable.db.gz", destfile="freqTable.db.gz", method="curl")
  }
  gunzip("freqTable.db.gz", destname="freqTable.db")
}

if (!file.exists("profanity.txt")) {
  download.file("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", destfile="profanity.txt", method="curl")
}

profanity <- readLines("profanity.txt")
