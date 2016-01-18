
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
  dataset <- gsub("[0-9'’`\u0080-\uFFFF]", "", dataset) # Remove non-latin characters and numbers and apostrophes
  dataset <- gsub("[[:punct:][:space:][:cntrl:]]", " ", dataset) # Replace punctuation with space (avoid making new words - we are stripping numbers anyway)
  dataset <- removeWords(dataset, profanity) # Remove profanity
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
  dataset <- stemDocument(dataset, language="english") # Stem words
  dataset <- stripWhitespace(dataset) # Remove extra whitespace
  dataset <- trimws(dataset) # Trim leading/trailing ws
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
    if (nrow(v) > 0) {
      v$PredictWord <- lookupWordByID(db, v$Predict)
    }
    else {
      v <- NULL
    }
    v
}

doPrediction <- function(tokens) {
  db <- dbConnect(SQLite(), "freqTable.db", flags=SQLITE_RO)
  #print(tokens)
  # Get word IDs for known tokens, and reverse so last word is first
  idx <- rev(lookupWord(db, tokens))
  predict <- NULL
  if (length(idx) >= 4) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable4 WHERE Word4=? AND Word3=? AND Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]],zz=idx[[4]]))
      predict <- findPredict(db, v)
      print(paste("pred4:", tokens, rev(idx)))
      print(predict)
  }
  if (is.null(predict) && (length(idx) >= 3)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable3 WHERE Word3=? AND Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]]))
      predict <- findPredict(db, v)
      print(paste("pred3:", tokens, rev(idx)))
      print(predict)
  }
  if (is.null(predict) && (length(idx) >= 2)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable2 WHERE Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]]))
      predict <- findPredict(db, v)
      print(paste("pred2:", tokens, rev(idx)))
      print(predict)
  }
  if (is.null(predict) && (length(idx) >= 1)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable1 WHERE Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]]))
      predict <- findPredict(db, v)
      print(paste("pred1:", tokens, rev(idx)))
      print(predict)
  }
  dbDisconnect(db)
  predict
}

print("Start Shiny Server")
shinyServer(function(input, output) {
  prediction <- reactive({
        sentences <- lineToSentences(input$text)
        #print(sentences)
        predict = data.frame(Freq=0,Predict=-1,PredictWord="<no prediction>", stringsAsFactors = FALSE)
        scnt <- length(sentences)
        #print(scnt)
        if (scnt >= 1) {
          # Tokenize last sentence, since prediction is for after this...
          v <- tokenizeSentence(sentences[scnt])
          # Split by spaces
          v <- strsplit(v, " ", fixed=TRUE)[[1]]
          # Run prediction
          if (length(v) >= 1) {
              #withProgress({
                #setProgress("Running word prediction...")
                predict <- doPrediction(v)
              #})
          }
        }
        predict
    })     
    output$Prediction <- renderText({
      p <- prediction()
      print(p)
      p$PredictWord[[1]]
    })
    output$Prediction2 <- renderText({
      p <- prediction()
      #print(p)
      ifelse(nrow(p) >= 2, p$PredictWord[[2]], "")
    })
    output$Prediction3 <- renderText({
      p <- prediction()
      #print(p)
      ifelse(nrow(p) >= 3, p$PredictWord[[3]], "")
    })
    outputOptions(output, "Prediction2", suspendWhenHidden=FALSE)    
    outputOptions(output, "Prediction3", suspendWhenHidden=FALSE)    

    #wordcloud_rep <- repeatable(wordcloud)
    
    output$WordCloud <- renderPlot({
      p <- prediction()
      #print(p)
      if (is.null(p) || (p$Predict[[1]] < 0)) {
          NULL
      } else {
        wordcloud(p$PredictWord, p$Freq, colors=brewer.pal(8, "Dark2"), max.words=20, min.freq=1, rot.per = 0.3, random.color=TRUE)
      }
    })
    
})
print("Shiny Server Started")