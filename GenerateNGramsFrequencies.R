#
# This script performs some basic n-gram generation and frequency analysis
#
options(java.parameters = "-Xmx8192m")
source("./LoadAndClean.R")

buildNGramFrequency <- function(dataset, n) {
  if (n == 1) {
    tokens <- WordTokenizer(dataset)
  } else {
    tokens <- NGramTokenizer(dataset, Weka_control(min = n, max = n))
  }
  f <- data.frame(table(tokens))
  fsort <- f[order(f$Freq, decreasing=TRUE),]
  fsort
}
build1ToNGramFrequency <- function(dataset, n) {
  rslt <- lapply(1:n, function(i) buildNGramFrequency(dataset, i))
  names(rslt) <- paste0(1:n, "-Gram")
  rslt
}
if (!file.exists("./data/en_US.freq.RData")) {
  blogsFreq <- build1ToNGramFrequency(blogsCorpus, 5)
  newsFreq <- build1ToNGramFrequency(newsCorpus, 5)
  tweetFreq <- build1ToNGramFrequency(tweetCorpus, 5)
  combinedFreq <- build1ToNGramFrequency(c(tweetCorpus, newsCorpus, blogsCorpus), 5)
  save(blogsFreq, newsFreq, tweetFreq, combinedFreq, file="./data/en_US.freq.RData")
} else {
  load("./data/en_US.freq.RData")
}