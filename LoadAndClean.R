#
# In this script, we load the sentence data and build corpus for each data source
#
source("./DownloadAndMakeSentences.R")

# Build corpus for each
require(tm)
require(RWeka)
options(mc.cores=1)
boguschars <- c("\u0092", "\u0093", "\u0094", "\u0095", "\u0096", "\u0097")
makeCorpus <- function(dataset, dsname) {
  corpus <- VCorpus(VectorSource(dataset))
  corpus <- tm_map(corpus, content_transformer(tolower)) # Make all lower case
  corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers) # Remove numbers
  corpus <- tm_map(corpus, stripWhitespace) # Remove extra whitespace
  corpus <- tm_map(corpus, removeWords, profanity) # Remove profanity
  corpus <- tm_map(corpus, removeWords, boguschars) # Remove bogus characters
  corpus <- tm_map(corpus, stripWhitespace) # Remove extra whitespace (in case extra from profanity filter)
  unlist(lapply(corpus, "[", "content"), use.names=FALSE)
}

# Percentage of data sampled and used for corpus
samplePercent <- 5.0

# Load profanity word list
profanity <- readLines("./data/profanity.txt")

if (!file.exists("./data/en_US.corpus.RData")) {
  # Load sentence structured data (one sentence per line)
  blogSentence <- readLines("./data/en_US.blogs.sentences.txt.gz", encoding="UTF-8")
  newsSentence <- readLines("./data/en_US.news.sentences.txt.gz", encoding="UTF-8")
  tweetSentence <- readLines("./data/en_US.twitter.sentences.txt.gz", encoding="UTF-8")
  
  # Sample the sentence data (too big for us to process practically)
  set.seed(123456)
  blogSample <- sample(blogSentence, as.integer(length(blogSentence) * 0.01 * samplePercent))
  remove(blogSentence)
  newsSample <- sample(newsSentence, as.integer(length(newsSentence) * 0.01 * samplePercent))
  remove(newsSentence)
  tweetSample <- sample(tweetSentence, as.integer(length(tweetSentence) * 0.01 * samplePercent))
  remove(tweetSentence)
  
  blogsCorpus <- makeCorpus(blogSample)
  newsCorpus <- makeCorpus(newsSample)
  tweetCorpus <- makeCorpus(tweetSample)
  save(blogsCorpus, newsCorpus, tweetCorpus, blogSample, newsSample, tweetSample, file="./data/en_US.corpus.RData", compress=TRUE, ascii=FALSE)
} else {
  # Load cached corpus and sample values
  load("./data/en_US.corpus.RData")
}
