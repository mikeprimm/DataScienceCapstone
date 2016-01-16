suppressPackageStartupMessages(c(
    library(shiny),
    library(RSQLite),
    require(NLP),
    require(openNLP),
    require(tm),
    require(RWeka),
    require(R.utils),
    require(SnowballC)
))

if (!file.exists("freqTable.db")) {
    if (!file.exists("freqTable.db.gz")) {
        download.file("http://mikeprimm.com/DataScienceCapstone/freqTable.db.gz", destfile="freqTable.db.gz", method="curl")
    }
    gunzip("freqTable.db.gz", destname="freqTable.db")
    file.remove("freqTable.db.gz")
}

if (!file.exists("profanity.txt")) {
  download.file("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", destfile="profanity.txt", method="curl")
}

profanity <- readLines("profanity.txt")

# Functions for converting line that may be multiple sentences into sentences
sent_annotator <- Maxent_Sent_Token_Annotator(language="en")

lineToSentences <- function(s) {
    if(s != "") {
      s <- as.String(s)
      v <- s[annotate(s, sent_annotator)]
      s <- as.character(v)
    }
    s
}

trimws <- function(x){
    res <- x
    res <- sub('^\\s+', '',res)
    res <- sub('\\s+$', '',res)
    res
}
# Build tokenized version of each sentence (freom TokenizeSentences.R - keep in sync)
tokenizeSentence <- function(dataset) {
  dataset <- as.character(dataset)
  dataset <- tolower(dataset) # Make all lower case
  dataset <- gsub("[0-9'’\u0080-\uFFFF]", "", dataset) # Remove non-latin characters and numbers and apostrophes
  dataset <- gsub("[[:punct:][:space:]]", " ", dataset) # Replace punctuation with space (avoid making new words - we are stripping numbers anyway)
  dataset <- removeWords(dataset, profanity) # Remove profanity
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
  v <- dataset # Grab copy before stopwords
  dataset <- removeWords(dataset, stopwords("english")) # Remove stop words
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
  v2 <- dataset # Grab version without stemming
  dataset <- stemDocument(dataset, language="english") # Stem words
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
  #paste(v, v2, dataset, sep=":")
  dataset
}

lookupWord <- function(db, words) {
  v <- dbGetPreparedQuery(db, "SELECT Word, Ind from UniqueWords where Word = ?", data.frame(x=words))
  # Scrub out unknown words - we'll match on last known words
  v[!is.na(v$Ind),]$Ind
}

lookupWordByID <- function(db, ids) {
    v <- dbGetPreparedQuery(db, "SELECT Word, Ind from UniqueWords where Ind = ?", data.frame(x=ids))
    v$Word
}

findPredict <- function(db, v) {
    pred <- NULL
    if (nrow(v) > 0) {
        pred <- lookupWordByID(db, v$Predict[[nrow(v)]])
    }
    pred
}

doPrediction <- function(tokens) {
  db <- dbConnect(SQLite(), "freqTable.db", flags=SQLITE_RO)
  # Get word IDs for known tokens, and reverse so last word is first
  idx <- rev(lookupWord(db, tokens))
  predict <- NULL
  if (length(idx) >= 4) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, sum(Freq) from freqTable WHERE Word4=? AND Word3=? AND Word2=? AND Word1=? GROUP BY Predict ORDER BY sum(Freq)", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]],zz=idx[[4]]))
      predict <- findPredict(db, v)
  }
  if (is.null(predict) && (length(idx) >= 3)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, sum(Freq) from freqTable WHERE Word4=? AND Word3=? AND Word2=? GROUP BY Predict ORDER BY sum(Freq)", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]]))
      predict <- findPredict(db, v)
  }
  if (is.null(predict) && (length(idx) >= 2)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, sum(Freq) from freqTable WHERE Word4=? AND Word3=? GROUP BY Predict ORDER BY sum(Freq)", data.frame(x=idx[[1]],y=idx[[2]]))
      predict <- findPredict(db, v)
  }
  if (is.null(predict) && (length(idx) >= 1)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, sum(Freq) from freqTable WHERE Word4=? GROUP BY Predict ORDER BY sum(Freq)", data.frame(x=idx[[1]]))
      predict <- findPredict(db, v)
  }
  dbDisconnect(db)
  if (is.null(predict)) {
      predict <- "<no prediction>"
  }
  predict
}

print("Start Shiny Server")
shinyServer(function(input, output) {

    output$Prediction <- renderText({
        sentences <- lineToSentences(input$text)
        predict = "";
        scnt <- length(sentences)
        if (scnt >= 1) {
          # Tokenize last sentence, since prediction is for after this...
          v <- tokenizeSentence(sentences[scnt])
          # Split by spaces
          v <- strsplit(v, " ", fixed=TRUE)[[1]]
          # Run prediction
          if (length(v) >= 1) {
              predict <- doPrediction(v)
          }
        }
        predict
    })     
})
print("Shiny Server Started")