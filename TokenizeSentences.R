source("./DownloadAndMakeSentences.R")

require(NLP)
require(openNLP)
require(tm)
require(RWeka)
options(mc.cores=1)
profanity <- readLines("./data/profanity.txt")

# Build tokenized version of each sentence
tokenizeSentence <- function(dataset) {
  dataset <- tolower(dataset) # Make all lower case
  dataset <- gsub("[0-9'’`\u0080-\uFFFF]", "", dataset) # Remove non-latin characters and numbers and apostrophes
  dataset <- gsub("[[:punct:][:space:][:cntrl:]]", " ", dataset) # Replace punctuation with space (avoid making new words - we are stripping numbers anyway)
  dataset <- removeWords(dataset, profanity) # Remove profanity
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
  v <- dataset # Grab version without stemming
  dataset <- stemDocument(dataset, language="english") # Stem words
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
  paste(v, dataset, sep=":")
}

tokenizeSentences <- function(infile, outfile) {
  cl <- makeCluster(mc <- getOption("cl.cores", 8))
  clusterEvalQ(cl, {
    require(tm)
    require(RWeka)
    options(mc.cores=1)
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
    print(paste(Sys.time(),"Processed", cnt, "lines, ", cntout, "written"))
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
