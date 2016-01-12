
require(tm)
require(NLP)
require(parallel)
require(RSQLite)

workerCount <- 8

nodeId <- 0

initDB <- function() {
  con <- dbConnect(RSQLite::SQLite(), paste0("./data/frequency", nodeId, ".db"))
  tabs <- dbListTables(con)
  for (t in tabs) {
    dbRemoveTable(con, t)
  }
  dbSendQuery(con, "CREATE TABLE FREQ1 (NGRAM TEXT, PREDICT TEXT, FREQUENCY INT, PRIMARY KEY (NGRAM, PREDICT))")
  dbSendQuery(con, "CREATE TABLE FREQ2 (NGRAM TEXT, PREDICT TEXT, FREQUENCY INT, PRIMARY KEY (NGRAM, PREDICT))")
  dbSendQuery(con, "CREATE TABLE FREQ3 (NGRAM TEXT, PREDICT TEXT, FREQUENCY INT, PRIMARY KEY (NGRAM, PREDICT))")
  dbSendQuery(con, "CREATE TABLE FREQ4 (NGRAM TEXT, PREDICT TEXT, FREQUENCY INT, PRIMARY KEY (NGRAM, PREDICT))")
  con
}

closeDB <- function(con) {
  #sqliteCopyDatabase(con, paste0("./data/frequency", nodeId, ".db"))
  dbDisconnect(con)
}

addToFrequencyList <- function(ngram, predict, n) {
  tryCatch({
    dbGetPreparedQuery(dbCon, paste0("INSERT INTO FREQ", (n-1), " VALUES(?, ?, 1)"), data.frame(x=ngram,y=predict))
  }, error = function(e) {
    rslt <- dbGetPreparedQuery(dbCon, paste0("SELECT FREQUENCY FROM FREQ", (n-1), " WHERE (NGRAM = ?) AND (PREDICT = ?)"), data.frame(x=ngram,y=predict))
    if (length(rslt) == 1) {
      dbGetPreparedQuery(dbCon, paste0("UPDATE FREQ", (n-1), " SET FREQUENCY=? WHERE (NGRAM = ?) AND (PREDICT = ?)"), data.frame(f=rslt[1]$FREQUENCY+1,x=ngram,y=predict))
    }      
  })                           
}

processTokenLine <- function(poststem, stemdict) {
  # Get N-grams for post-stem line and build predictions
  ng <- ngrams(poststem, 2:5)
  if(length(ng) > 0) {
    n <- vapply(ng, length, 0)
    predict <- mapply(function(f, nn) { f[nn] }, ng, n) # Get last words of ngrams
    #predict <- stemCompletion(predict, stemdict, type="first") # restore stem
    predict <- ifelse(is.na(idx <- match(predict, poststem)), predict, stemdict[idx])
    ngkeylist <- mapply(function(f, nn) { f[1:(nn-1)] }, ng, n)
    ngkey <- vapply(ngkeylist, paste, "", collapse = " ")
    mapply(addToFrequencyList, ngkey, predict, n)
  }
}

processLines <- function(line) {
  parts <- strsplit(line, ":", fixed=TRUE)[[1]] # Split parts (1=sentence, 2=after stopwords, 3=after stem)
  stemdict <- strsplit(parts[2], " ", fixed=TRUE)[[1]] # Make stem completion dictionary from post stopwords
  poststem <- strsplit(parts[3], " ", fixed=TRUE)[[1]] # Start with post-stem line
  processTokenLine(poststem, stemdict)
}

processTokenFile <- function(parlist, infile) {
  print(paste(Sys.time(), "Processing tokens from", infile))
  inp <- file(infile, "r", encoding="UTF-8")
  cnt <- 0
  rslt <- ""
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 20000, skipNul=TRUE)
    parLapply(cl, rslt, processLines)
    #lapply(rslt, processLines)
    cnt <- cnt + length(rslt)
    print(paste(Sys.time(), "Processed", cnt, "lines"))
  }
  close(inp)
}

#buildUniqueWordList <- function(infile) {
#  print(paste(Sys.time(), "Processing unique words from", infile))
#  inp <- file(infile, "r", encoding="UTF-8")
#  cnt <- 0
#  rslt <- ""
#  env <- new.env(hash = TRUE, parent = emptyenv(), size = 200000)
#  while(length(rslt) > 0) {
#    rslt <- readLines(inp, 20000, skipNul=TRUE)
#    parts <- strsplit(rslt, ":", fixed=TRUE) # Split parts (1=sentence, 2=after stopwords, 3=after stem)
#    stemdict <- lapply(parts, function(f) { strsplit(f[2], " ", fixed=TRUE)[[1]] }) # Make stem completion dictionary from post stopwords
#    poststem <- lapply(parts, function(f) { strsplit(f[3], " ", fixed=TRUE)[[1]] }) # Start with post-stem line
#    lapply(stemdict, function(f) { vapply(f, function(ff) { env[[ff]] <- TRUE; ff }, "") })
#    lapply(poststem, function(f) { vapply(f, function(ff) { env[[ff]] <- TRUE; ff }, "") })
#    cnt <- cnt + length(rslt)
#    print(paste(Sys.time(), "Processed", cnt, "lines"))
#  }
#  close(inp)
#  names(env)
#}  

# Startup cluster
cl <- makeCluster(mc <- getOption("cl.cores", workerCount), outfile="")
clusterApply(cl, 1:workerCount, function(id) { nodeId <<- id })
clusterExport(cl, c("addToFrequencyList", "initDB", "closeDB", "processTokenLine", "processLines"))
clusterEvalQ(cl, {
  require(tm)
  require(RSQLite)
  dbCon <- initDB()
})

processTokenFile(predictTable, "./data/en_US.blogs.tokens.txt.gz")
#save(predictTable, file="./data/predictTable1.RData")
processTokenFile(predictTable, "./data/en_US.news.tokens.txt.gz")
#save(predictTable, file="./data/predictTable2.RData")
processTokenFile(predictTable, "./data/en_US.twitter.tokens.txt.gz")
#save(predictTable, file="./data/predictTable3.RData")

# Cleanup cluster
clusterEvalQ(cl, {
  closeDB(dbCon)
  rm(dbCon)
})
stopCluster(cl)


#if(!file.exists("./data/uniqueWordList.RData")) {
#  blogWords <- buildUniqueWordList("./data/en_US.blogs.tokens.txt.gz")
#  newsWords <- buildUniqueWordList("./data/en_US.news.tokens.txt.gz")
#  twitterWords <- buildUniqueWordList("./data/en_US.twitter.tokens.txt.gz")
#  allWords <- sort(unique(c(blogWords,newsWords,twitterWords)))
#  rm(blogWords)
#  rm(newsWords)
#  rm(twitterWords)
#  save(allWords, file="./data/uniqueWordList.RData")
#} else {
#  load("./data/uniqueWordList.RData")
#}
