# Script for fetching and loading raw data
#  This script will save the resulting objects, so that they can be more quickly loaded (or reloaded)
# later, as these data are quite large.
#
# Make directory for work data
dir.create("./data", showWarnings=FALSE)
# URL for data source
URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# If data set not downloaded already, fetch it
if (!file.exists("./data/Coursera-SwiftKey.zip")) {
  download.file(URL, destfile = "./data/Coursera-SwiftKey.zip", method="curl")
}
# If data set not extracted already, extract it
if (!file.exists("./data/final/en_US/en_US.blogs.txt")) {
  unzip("./data/Coursera-SwiftKey.zip", exdir="./data")
}

# Also, get list of profanity from ShutterStock github
if (!file.exists("./data/profanity.txt")) {
  download.file("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", destfile="./data/profanity.txt", method="curl")
}

# Next, we are going to process the record-oriented files into sentence oriented ones, since 
# we don't actually care about anything except sentences.  This is expensive, so only do it if needed.
# We also fire up a parallel cluster to handle this, as it is very long otherwise.
require(parallel)
require(NLP)
require(openNLP)
lineToSentences <- function(s) {
  s <- as.String(s)
  s[annotate(s, sent_annotator)]
}
convertRecordsToSentences <- function(infile, outfile) {
  cl <- makeCluster(mc <- getOption("cl.cores", 8))
  clusterEvalQ(cl, {
    require(openNLP)
    require(NLP)
    sent_annotator <- Maxent_Sent_Token_Annotator(language="en")
  })
  inp <- file(infile, "r", encoding="UTF-8")
  outp <- gzfile(outfile, "wt+", encoding="UTF-8")
  rslt <- ""
  cnt <- 0
  cntout <- 0
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 10000, skipNul=TRUE)
    v <- parLapply(cl, rslt, lineToSentences)
    lines <- Reduce(c, v)
    if (length(lines) > 0) {
      writeLines(lines, outp)
    }
    cnt <- cnt + length(rslt)
    cntout <- cntout + length(lines)
    #print(paste("Processed", cnt, "lines, ", cntout, "written"))
  }
  close(inp)
  close(outp)
  stopCluster(cl)
}

if (!file.exists("./data/en_US.blogs.sentences.txt.gz")) {
  convertRecordsToSentences("./data/final/en_US/en_US.blogs.txt", "./data/en_US.blogs.sentences.txt.gz")
}
if (!file.exists("./data/en_US.news.sentences.txt.gz")) {
  convertRecordsToSentences("./data/final/en_US/en_US.news.txt", "./data/en_US.news.sentences.txt.gz")
}
if (!file.exists("./data/en_US.twitter.sentences.txt.gz")) {
  convertRecordsToSentences("./data/final/en_US/en_US.twitter.txt", "./data/en_US.twitter.sentences.txt.gz")
}
