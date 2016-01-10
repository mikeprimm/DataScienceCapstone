
require(tm)
require(NLP)

predictTable <- list(new.env(hash=TRUE, parent=emptyenv(), size=50000),
                     new.env(hash=TRUE, parent=emptyenv(), size=50000),
                     new.env(hash=TRUE, parent=emptyenv(), size=50000),
                     new.env(hash=TRUE, parent=emptyenv(), size=50000))

addToFrequencyList <- function(ngram, predict, n) {
  tab <- predictTable[[n-1]]
  row <- tab[[ngram]]
  if (is.null(row)) {
    row <- list()
    row[[predict]] <- 1
  } else if (is.null(row[[predict]])) {
    row[[predict]] <- 1
  } else {
    row[[predict]] <- row[[predict]] + 1
  }
  tab[[ngram]] <- row
}

processTokenLine <- function(poststem, stemdict) {
  # Get N-grams for post-stem line and build predictions
  ng <- ngrams(poststem, 2:5)
  if(length(ng) > 0) {
    n <- vapply(ng, length, 0)
    predict <- mapply(function(f, nn) { f[nn] }, ng, n) # Get last words of ngrams
    predict <- stemCompletion(predict, stemdict, type="first") # restore stem
    ngkey <- vapply(mapply(function(f, nn) { f[1:(nn-1)] }, ng, n), paste, "", collapse = " ")
    mapply(addToFrequencyList, ngkey, predict, n)
  }
}

processTokenFile <- function(parlist, infile) {
  print(paste(Sys.time(), "Processing tokens from", infile))
  inp <- file(infile, "r", encoding="UTF-8")
  cnt <- 0
  rslt <- ""
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 5000, skipNul=TRUE)
    parts <- strsplit(rslt, ":", fixed=TRUE) # Split parts (1=sentence, 2=after stopwords, 3=after stem)
    stemdict <- lapply(parts, function(f) { strsplit(f[2], " ", fixed=TRUE)[[1]] }) # Make stem completion dictionary from post stopwords
    poststem <- lapply(parts, function(f) { strsplit(f[3], " ", fixed=TRUE)[[1]] }) # Start with post-stem line
    #for (idx in 1:length(rslt)) {
    #  processTokenLine(poststem[[idx]], stemdict[[idx]])
    #}
    mapply(processTokenLine, poststem, stemdict)
    cnt <- cnt + length(rslt)
    print(paste(Sys.time(), "Processed", cnt, "lines"))
  }
  close(inp)
}

buildUniqueWordList <- function(infile) {
  print(paste(Sys.time(), "Processing unique words from", infile))
  inp <- file(infile, "r", encoding="UTF-8")
  cnt <- 0
  rslt <- ""
  env <- new.env(hash = TRUE, parent = emptyenv(), size = 200000)
  while(length(rslt) > 0) {
    rslt <- readLines(inp, 5000, skipNul=TRUE)
    parts <- strsplit(rslt, ":", fixed=TRUE) # Split parts (1=sentence, 2=after stopwords, 3=after stem)
    stemdict <- lapply(parts, function(f) { strsplit(f[2], " ", fixed=TRUE)[[1]] }) # Make stem completion dictionary from post stopwords
    poststem <- lapply(parts, function(f) { strsplit(f[3], " ", fixed=TRUE)[[1]] }) # Start with post-stem line
    lapply(stemdict, function(f) { vapply(f, function(ff) { env[[ff]] <- TRUE; ff }, "") })
    lapply(poststem, function(f) { vapply(f, function(ff) { env[[ff]] <- TRUE; ff }, "") })
    cnt <- cnt + length(rslt)
    print(paste(Sys.time(), "Processed", cnt, "lines"))
  }
  close(inp)
  names(env)
}  

processTokenFile(predictTable, "./data/en_US.blogs.tokens.txt.gz")
save(predictTable, file="./data/predictTable1.RData")
processTokenFile(predictTable, "./data/en_US.news.tokens.txt.gz")
save(predictTable, file="./data/predictTable2.RData")
processTokenFile(predictTable, "./data/en_US.twitter.tokens.txt.gz")
save(predictTable, file="./data/predictTable3.RData")
