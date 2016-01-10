source("./DownloadAndMakeSentences.R")

require(NLP)
require(openNLP)
require(tm)
require(RWeka)
options(mc.cores=1)
boguschars <- c("\u0092", "\u0093", "\u0094", "\u0095", "\u0096", "\u0097")
profanity <- readLines("./data/profanity.txt")

# Build tokenized version of each sentence
tokenizeSentence <- function(dataset) {
  corpus <- VCorpus(VectorSource(dataset))
  corpus <- tm_map(corpus, content_transformer(tolower)) # Make all lower case
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = FALSE) # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers) # Remove numbers
  corpus <- tm_map(corpus, removeWords, profanity) # Remove profanity
  corpus <- tm_map(corpus, removeWords, boguschars) # Remove bogus characters
  corpus <- tm_map(corpus, stripWhitespace) # Remove extra whitespace
  v <- trimws(unlist(lapply(corpus, "[", "content"), use.names=FALSE)) # Grab version with stop words
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stop worlds
  corpus <- tm_map(corpus, stripWhitespace)
  v2 <- trimws(unlist(lapply(corpus, "[", "content"), use.names=FALSE)) # Grab version without stemming
  corpus <- tm_map(corpus, stemDocument, language = "english") # Stem words
  corpus <- tm_map(corpus, stripWhitespace)
  paste(v, v2, trimws(unlist(lapply(corpus, "[", "content"), use.names=FALSE)), sep=":")
}

tokenizeSentences <- function(infile, outfile) {
  cl <- makeCluster(mc <- getOption("cl.cores", 8))
  clusterEvalQ(cl, {
    require(tm)
    require(RWeka)
    options(mc.cores=1)
    boguschars <- c("\u0092", "\u0093", "\u0094", "\u0095", "\u0096", "\u0097")
    profanity <- readLines("./data/profanity.txt")
  })
  inp <- file(infile, "r", encoding="UTF-8")
  outp <- gzfile(outfile, "wt+", encoding="UTF-8")
  rslt <- ""
  cnt <- 0
  cntout <- 0
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 50000, skipNul=TRUE)
    v <- parLapply(cl, rslt, tokenizeSentence)
    lines <- Reduce(c, v)
    if (length(lines) > 0) {
      writeLines(lines, outp)
    }
    cnt <- cnt + length(rslt)
    cntout <- cntout + length(lines)
    print(paste("Processed", cnt, "lines, ", cntout, "written"))
  }
  close(inp)
  close(outp)
  stopCluster(cl)
}

if (!file.exists("./data/en_US.blogs.tokens.txt.gz")) {
  tokenizeSentences("./data/en_US.blogs.sentences.txt.gz", "./data/en_US.blogs.tokens.txt.gz")
}
if (!file.exists("./data/en_US.news.tokens.txt.gz")) {
  tokenizeSentences("./data/en_US.news.sentences.txt.gz", "./data/en_US.news.tokens.txt.gz")
}
if (!file.exists("./data/en_US.twitter.tokens.txt.gz")) {
  tokenizeSentences("./data/en_US.twitter.sentences.txt.gz", "./data/en_US.twitter.tokens.txt.gz")
}
