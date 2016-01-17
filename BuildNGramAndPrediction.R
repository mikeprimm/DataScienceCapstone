
require(tm)
require(NLP)
require(data.table)

nodeId <- 0

addToFrequencyList <- function(ngram, predict) {
  writeLines(paste0(ngram, " :", predict), dbfile)
}

processTokenLine <- function(poststem, stemdict) {
  # Get 5-grams for whole sentence, and add on truncated 4, 3 and 2
  pslen <- length(poststem)
  ng <- list()
  if (pslen >= 5) {
    ng <- c(ngrams(poststem, 5), list(poststem[1:4], poststem[1:3], poststem[1:2]))
  } else if (pslen == 4) {
    ng <- list(poststem[1:4], poststem[1:3], poststem[1:2])
  } else if (pslen == 3) {
    ng <- list(poststem[1:3], poststem[1:2])
  } else if (pslen == 2) {
    ng <- list(poststem[1:2])
  }
  else {
    ng <- list()
  }
  if(length(ng) > 0) {
    n <- vapply(ng, length, 0)
    predict <- mapply(function(f, nn) { f[nn] }, ng, n) # Get last words of ngrams
    predict <- ifelse(is.na(idx <- match(predict, poststem)), predict, stemdict[idx])
    ngkeylist <- mapply(function(f, nn) { f[(nn-1):1] }, ng, n)
    ngkey <- vapply(ngkeylist, paste, "", collapse = " ")
    addToFrequencyList(ngkey, predict)
  }
}

processLines <- function(line) {
  parts <- strsplit(line, ":", fixed=TRUE)[[1]] # Split parts (1=sentence, 2=after stopwords, 3=after stem)
  stemdict <- strsplit(parts[2], " ", fixed=TRUE)[[1]] # Make stem completion dictionary from post stopwords
  poststem <- strsplit(parts[3], " ", fixed=TRUE)[[1]] # Start with post-stem line
  processTokenLine(poststem, stemdict)
}

processTokenFile <- function(infile) {
  print(paste(Sys.time(), "Processing tokens from", infile))
  inp <- file(infile, "r", encoding="UTF-8")
  cnt <- 0
  rslt <- ""
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 20000, skipNul=TRUE)
    lapply(rslt, processLines)
    cnt <- cnt + length(rslt)
    print(paste(Sys.time(), "Processed", cnt, "lines"))
  }
  close(inp)
}

if (!file.exists("./data/ngram_pred.txt")) {
  dbfile <- file("./data/ngram_pred.txt", open = "wt+")

  processTokenFile("./data/en_US.blogs.tokens.txt.gz")
  processTokenFile("./data/en_US.news.tokens.txt.gz")
  processTokenFile("./data/en_US.twitter.tokens.txt.gz")
  close(dbfile)
  rm(dbfile)
}
# Make sorted version of file: much easier to count frequencies, and faster than R code
#system("sort -o ./data/ngram_pred.sorted.txt ./data/ngram_pred.txt")

#dbfile <- file("./data/ngram_pred.sorted.txt", open = "rt")
# Load rows and make frequency table
if(!file.exists("./data/freqTable.RData")) {
  ftable <- data.table(table(readLines("./data/ngram_pred.txt")))
  ngramPredict <- strsplit(ftable$V1, ":", fixed=TRUE)
  Predict <- vapply(ngramPredict, function(v) v[[2]], "")
  Ngrams <- lapply(ngramPredict, function(v) strsplit(v[[1]], " ", fixed=TRUE)[[1]])
  rm(ngramPredict)
  Freq <- ftable$N
  Word4 <- vapply(Ngrams, function(v) ifelse(length(v) >= 1, v[[1]], NA_character_), "")
  Word3 <- vapply(Ngrams, function(v) ifelse(length(v) >= 2, v[[2]], NA_character_), "")
  Word2 <- vapply(Ngrams, function(v) ifelse(length(v) >= 3, v[[3]], NA_character_), "")
  Word1 <- vapply(Ngrams, function(v) ifelse(length(v) >= 4, v[[4]], NA_character_), "")
  rm(Ngrams)
  rm(ftable)
  UniqueWords <- sort(unique(c(Word4,Word3,Word2,Word1,Predict)))
  Word4 <- as.numeric(factor(Word4, UniqueWords))
  Word3 <- as.numeric(factor(Word3, UniqueWords))
  Word2 <- as.numeric(factor(Word2, UniqueWords))
  Word1 <- as.numeric(factor(Word1, UniqueWords))
  Predict <- as.numeric(factor(Predict, UniqueWords))
  freqTable <- data.table(Word4 = Word4, Word3 = Word3, Word2 = Word2, Word1 = Word1, Predict = Predict, Freq = Freq)
  rm(Word4)
  rm(Word3)
  rm(Word2)
  rm(Word1)
  rm(Predict)
  rm(Freq)
  setkey(freqTable, Word4, Word3, Word2, Word1, Predict)
  save(freqTable, UniqueWords, file="./data/freqTable.RData", compress=TRUE)
} else {
  load("./data/freqTable.RData")
}

# Build SQLite prediction table DB
if (!file.exists("./data/freqTable.db")) {
  require(RSQLite)
  db <- dbConnect(SQLite(), "./data/freqTable.db")
  dbGetQuery(db, "CREATE TABLE UniqueWords (Word TEXT, Ind INT, PRIMARY KEY (Word))")
  dbGetQuery(db, "CREATE TABLE freqTable (Word4 INT, Word3 INT, Word2 INT, Word1 INT, Predict INT, Freq INT, PRIMARY KEY (Word4,Word3,Word2,Word1,Predict))")
  dbWriteTable(db, "UniqueWords", data.table(Word=UniqueWords,Ind=c(1:length(UniqueWords))), append=TRUE)
  dbWriteTable(db, "freqTable", freqTable, append=TRUE)
  dbDisconnect(db)
}
if (!file.exists("./data/freqTable.db.gz")) {
  require(R.utils)
  gzip("./data/freqTable.db", destname="./data/freqTable.db.gz", remove=FALSE)
}
