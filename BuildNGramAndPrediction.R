source("./TokenizeSentences.R")

require(tm)
require(NLP)
require(data.table)

nodeId <- 0

addToFrequencyList <- function(ngram, predict, outfile) {
  writeLines(paste0(ngram, " :", predict), outfile)
}

processTokenLine <- function(poststem, stemdict, n, outfile) {
  # Get N-grams for whole sentence
  ng <- ngrams(poststem, n+1)
  if(length(ng) > 0) {
    predict <- vapply(ng, function(f) f[n+1], "") # Get last words of ngrams
    predict <- ifelse(is.na(idx <- match(predict, poststem)), predict, stemdict[idx])
    ngkeylist <- lapply(ng, function(f) { f[n:1] })
    ngkey <- vapply(ngkeylist, paste, "", collapse = " ")
    addToFrequencyList(ngkey, predict, outfile)
  }
}

processLines <- function(line, dbfile1, dbfile2, dbfile3, dbfile4, dbfile5) {
  parts <- strsplit(line, ":", fixed=TRUE)[[1]] # Split parts (1=sentence, 2=after stem)
  stemdict <- strsplit(parts[1], " ", fixed=TRUE)[[1]] # Make stem completion dictionary from post stopwords
  poststem <- strsplit(parts[2], " ", fixed=TRUE)[[1]] # Start with post-stem line
  processTokenLine(poststem, stemdict, 1, dbfile1)
  processTokenLine(poststem, stemdict, 2, dbfile2)
  processTokenLine(poststem, stemdict, 3, dbfile3)
  processTokenLine(poststem, stemdict, 4, dbfile4)
  processTokenLine(poststem, stemdict, 5, dbfile5)
}

processTokenFile <- function(infile,dbfile1,dbfile2,dbfile3,dbfile4,dbfile5) {
  print(paste(Sys.time(), "Processing tokens from", infile))
  inp <- file(infile, "r", encoding="UTF-8")
  cnt <- 0
  rslt <- ""
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 20000, skipNul=TRUE)
    lapply(rslt, processLines, dbfile1,dbfile2,dbfile3,dbfile4,dbfile5)
    cnt <- cnt + length(rslt)
    print(paste(Sys.time(), "Processed", cnt, "lines"))
  }
  close(inp)
}

if (!file.exists("./data/ngram_pred4.txt.gz")) {
  dbfile1 <- gzfile("./data/ngram_pred1.txt.gz", open = "wt+")
  dbfile2 <- gzfile("./data/ngram_pred2.txt.gz", open = "wt+")
  dbfile3 <- gzfile("./data/ngram_pred3.txt.gz", open = "wt+")
  dbfile4 <- gzfile("./data/ngram_pred4.txt.gz", open = "wt+")
  dbfile5 <- gzfile("./data/ngram_pred5.txt.gz", open = "wt+")
  processTokenFile("./data/en_US.blogs.tokens.txt.gz",dbfile1,dbfile2,dbfile3,dbfile4,dbfile5)
  processTokenFile("./data/en_US.news.tokens.txt.gz",dbfile1,dbfile2,dbfile3,dbfile4,dbfile5)
  processTokenFile("./data/en_US.twitter.tokens.txt.gz",dbfile1,dbfile2,dbfile3,dbfile4,dbfile5)
  close(dbfile1)
  close(dbfile2)
  close(dbfile3)
  close(dbfile4)
  close(dbfile5)
}
# Load rows and make frequency table for single words
if(!file.exists("./data/freqTable1.RData")) {
  # Handle single words
  ftable <- data.table(table(readLines("./data/ngram_pred1.txt.gz")))
  ngramPredict <- strsplit(ftable$V1, ":", fixed=TRUE)
  Predict <- vapply(ngramPredict, function(v) v[[2]], "")
  Ngrams <- lapply(ngramPredict, function(v) strsplit(v[[1]], " ", fixed=TRUE)[[1]])
  rm(ngramPredict)
  Freq <- ftable$N
  Word1 <- vapply(Ngrams, function(v) v[[1]], "")
  rm(Ngrams)
  rm(ftable)
  # Grab unique words from single word case
  UniqueWords <- sort(unique(c(Word1,Predict)))
  # Map words to integers
  Word1 <- as.integer(factor(Word1, UniqueWords))
  Predict <- as.integer(factor(Predict, UniqueWords))
  # Build table  
  freqTable1 <- data.table(Word1 = Word1, Predict = Predict, Freq = Freq)
  rm(Word1)
  rm(Predict)
  rm(Freq)
  setkey(freqTable1, Word1, Predict)
  # Prune unique ones (lots of them, little predictive value)
  freqTable1 <- freqTable1[freqTable1$Freq > 1,]
  # Save it
  save(freqTable1, UniqueWords, file="./data/freqTable1.RData", compress=TRUE)
} else {
  load("./data/freqTable1.RData")
}
# Load rows and make frequency table for 2-grams
if(!file.exists("./data/freqTable2.RData")) {
  ftable <- data.table(table(readLines("./data/ngram_pred2.txt.gz")))
  ngramPredict <- strsplit(ftable$V1, ":", fixed=TRUE)
  Predict <- vapply(ngramPredict, function(v) v[[2]], "")
  Ngrams <- lapply(ngramPredict, function(v) strsplit(v[[1]], " ", fixed=TRUE)[[1]])
  rm(ngramPredict)
  Freq <- ftable$N
  Word1 <- vapply(Ngrams, function(v) v[[2]], "")
  Word2 <- vapply(Ngrams, function(v) v[[1]], "")
  rm(Ngrams)
  rm(ftable)
  # Map words to integers
  Word1 <- as.integer(factor(Word1, UniqueWords))
  Word2 <- as.integer(factor(Word2, UniqueWords))
  Predict <- as.integer(factor(Predict, UniqueWords))
  # Build table  
  freqTable2 <- data.table(Word2 = Word2, Word1 = Word1, Predict = Predict, Freq = Freq)
  rm(Word1)
  rm(Word2)
  rm(Predict)
  rm(Freq)
  setkey(freqTable2, Word2, Word1, Predict)
  # Prune unique ones (lots of them, little predictive value)
  freqTable2 <- freqTable2[freqTable2$Freq > 1,]
  # Save it
  save(freqTable2, file="./data/freqTable2.RData", compress=TRUE)
} else {
  load("./data/freqTable2.RData")
}

# Load rows and make frequency table for 3-grams
if(!file.exists("./data/freqTable3.RData")) {
  ftable <- data.table(table(readLines("./data/ngram_pred3.txt.gz")))
  ngramPredict <- strsplit(ftable$V1, ":", fixed=TRUE)
  Predict <- vapply(ngramPredict, function(v) v[[2]], "")
  Ngrams <- lapply(ngramPredict, function(v) strsplit(v[[1]], " ", fixed=TRUE)[[1]])
  rm(ngramPredict)
  Freq <- ftable$N
  Word1 <- vapply(Ngrams, function(v) v[[3]], "")
  Word2 <- vapply(Ngrams, function(v) v[[2]], "")
  Word3 <- vapply(Ngrams, function(v) v[[1]], "")
  rm(Ngrams)
  rm(ftable)
  # Map words to integers
  Word1 <- as.integer(factor(Word1, UniqueWords))
  Word2 <- as.integer(factor(Word2, UniqueWords))
  Word3 <- as.integer(factor(Word3, UniqueWords))
  Predict <- as.integer(factor(Predict, UniqueWords))
  # Build table  
  freqTable3 <- data.table(Word3 = Word3, Word2 = Word2, Word1 = Word1, Predict = Predict, Freq = Freq)
  rm(Word1)
  rm(Word2)
  rm(Word3)
  rm(Predict)
  rm(Freq)
  setkey(freqTable3, Word3, Word2, Word1, Predict)
  # Prune unique ones (lots of them, little predictive value)
  freqTable3 <- freqTable3[freqTable3$Freq > 1,]
  # Save it
  save(freqTable3, file="./data/freqTable3.RData", compress=TRUE)
} else {
  load("./data/freqTable3.RData")
}

# Load rows and make frequency table for 4-grams
if(!file.exists("./data/freqTable4.RData")) {
  ftable <- data.table(table(readLines("./data/ngram_pred4.txt.gz")))
  ngramPredict <- strsplit(ftable$V1, ":", fixed=TRUE)
  Predict <- vapply(ngramPredict, function(v) v[[2]], "")
  Ngrams <- lapply(ngramPredict, function(v) strsplit(v[[1]], " ", fixed=TRUE)[[1]])
  rm(ngramPredict)
  Freq <- ftable$N
  Word1 <- vapply(Ngrams, function(v) v[[4]], "")
  Word2 <- vapply(Ngrams, function(v) v[[3]], "")
  Word3 <- vapply(Ngrams, function(v) v[[2]], "")
  Word4 <- vapply(Ngrams, function(v) v[[1]], "")
  rm(Ngrams)
  rm(ftable)
  # Map words to integers
  Word1 <- as.integer(factor(Word1, UniqueWords))
  Word2 <- as.integer(factor(Word2, UniqueWords))
  Word3 <- as.integer(factor(Word3, UniqueWords))
  Word4 <- as.integer(factor(Word4, UniqueWords))
  Predict <- as.integer(factor(Predict, UniqueWords))
  # Build table  
  freqTable4 <- data.table(Word4 = Word4, Word3 = Word3, Word2 = Word2, Word1 = Word1, Predict = Predict, Freq = Freq)
  rm(Word1)
  rm(Word2)
  rm(Word3)
  rm(Word4)
  rm(Predict)
  rm(Freq)
  setkey(freqTable4, Word4, Word3, Word2, Word1, Predict)
  # Prune unique ones (lots of them, little predictive value)
  freqTable4 <- freqTable4[freqTable4$Freq > 1,]
  # Save it
  save(freqTable4, file="./data/freqTable4.RData", compress=TRUE)
} else {
  load("./data/freqTable4.RData")
}

#Load rows and make frequency table for 5-grams
if(!file.exists("./data/freqTable5.RData")) {
  ftable <- data.table(table(readLines("./data/ngram_pred5.txt.gz")))
  ngramPredict <- strsplit(ftable$V1, ":", fixed=TRUE)
  Predict <- vapply(ngramPredict, function(v) v[[2]], "")
  Ngrams <- lapply(ngramPredict, function(v) strsplit(v[[1]], " ", fixed=TRUE)[[1]])
  rm(ngramPredict)
  Freq <- ftable$N
  Word1 <- vapply(Ngrams, function(v) v[[5]], "")
  Word2 <- vapply(Ngrams, function(v) v[[4]], "")
  Word3 <- vapply(Ngrams, function(v) v[[3]], "")
  Word4 <- vapply(Ngrams, function(v) v[[2]], "")
  Word5 <- vapply(Ngrams, function(v) v[[1]], "")
  rm(Ngrams)
  rm(ftable)
  # Map words to integers
  Word1 <- as.integer(factor(Word1, UniqueWords))
  Word2 <- as.integer(factor(Word2, UniqueWords))
  Word3 <- as.integer(factor(Word3, UniqueWords))
  Word4 <- as.integer(factor(Word4, UniqueWords))
  Word5 <- as.integer(factor(Word5, UniqueWords))
  Predict <- as.integer(factor(Predict, UniqueWords))
  # Build table  
  freqTable5 <- data.table(Word5 = Word5, Word4 = Word4, Word3 = Word3, Word2 = Word2, Word1 = Word1, Predict = Predict, Freq = Freq)
  rm(Word1)
  rm(Word2)
  rm(Word3)
  rm(Word4)
  rm(Word5)
  rm(Predict)
  rm(Freq)
  setkey(freqTable5, Word5, Word4, Word3, Word2, Word1, Predict)
  # Prune unique ones (lots of them, little predictive value)
  freqTable5 <- freqTable5[freqTable5$Freq > 1,]
  # Save it
  save(freqTable5, file="./data/freqTable5.RData", compress=TRUE)
} else {
  load("./data/freqTable5.RData")
}

# Build SQLite prediction table DB
if (!file.exists("./ShinyApp/freqTable.db")) {
  require(RSQLite)
  db <- dbConnect(SQLite(), "./ShinyApp/freqTable.db")
  dbGetQuery(db, "CREATE TABLE UniqueWords (Word TEXT, Ind INT, PRIMARY KEY (Word))")
  dbGetQuery(db, "CREATE TABLE freqTable1 (Word1 INT, Predict INT, Freq INT, PRIMARY KEY (Word1,Predict))")
  dbGetQuery(db, "CREATE TABLE freqTable2 (Word2 INT, Word1 INT, Predict INT, Freq INT, PRIMARY KEY (Word2,Word1,Predict))")
  dbGetQuery(db, "CREATE TABLE freqTable3 (Word3 INT, Word2 INT, Word1 INT, Predict INT, Freq INT, PRIMARY KEY (Word3,Word2,Word1,Predict))")
  dbGetQuery(db, "CREATE TABLE freqTable4 (Word4 INT, Word3 INT, Word2 INT, Word1 INT, Predict INT, Freq INT, PRIMARY KEY (Word4,Word3,Word2,Word1,Predict))")
  dbGetQuery(db, "CREATE TABLE freqTable5 (Word5 INT, Word4 INT, Word3 INT, Word2 INT, Word1 INT, Predict INT, Freq INT, PRIMARY KEY (Word5,Word4,Word3,Word2,Word1,Predict))")
  dbWriteTable(db, "UniqueWords", data.table(Word=UniqueWords,Ind=c(1:length(UniqueWords))), append=TRUE)
  dbWriteTable(db, "freqTable1", freqTable1, append=TRUE)
  dbWriteTable(db, "freqTable2", freqTable2, append=TRUE)
  dbWriteTable(db, "freqTable3", freqTable3, append=TRUE)
  dbWriteTable(db, "freqTable4", freqTable4, append=TRUE)
  dbWriteTable(db, "freqTable5", freqTable5, append=TRUE)
  dbDisconnect(db)
}
if (!file.exists("./data/freqTable.db.gz")) {
  require(R.utils)
  gzip("./ShinyApp/freqTable.db", destname="./data/freqTable.db.gz", remove=FALSE)
}
