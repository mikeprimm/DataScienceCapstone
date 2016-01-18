
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
  dataset <- paste(stemDocument(strsplit(dataset, " ", fixed=TRUE)[[1]], language="english"), collapse=" ") # Stem words
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

doPrediction <- function(tokens, db) {
  # Get word IDs for known tokens, and reverse so last word is first
  idx <- rev(lookupWord(db, tokens))
  predict <- NULL
  if (is.null(predict) && length(idx) >= 5) {
    v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable5 WHERE Word5=? AND Word4=? AND Word3=? AND Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]],zz=idx[[4]],zzz=idx[[5]]))
    predict <- findPredict(db, v)
    #print(paste("pred5:", tokens, rev(idx), collapse = " "))
  }
  if (is.null(predict) && length(idx) >= 4) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable4 WHERE Word4=? AND Word3=? AND Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]],zz=idx[[4]]))
      predict <- findPredict(db, v)
      #print(paste("pred4:", tokens, rev(idx), collapse = " "))
  }
  if (is.null(predict) && (length(idx) >= 3)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable3 WHERE Word3=? AND Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]],z=idx[[3]]))
      predict <- findPredict(db, v)
      #print(paste("pred3:", tokens, rev(idx), collapse = " "))
  }
  if (is.null(predict) && (length(idx) >= 2)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable2 WHERE Word2=? AND Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]],y=idx[[2]]))
      predict <- findPredict(db, v)
      #print(paste("pred2:", tokens, rev(idx), collapse = " "))
  }
  if (is.null(predict) && (length(idx) >= 1)) {
      v <- dbGetPreparedQuery(db, "SELECT Predict, Freq from freqTable1 WHERE Word1=? ORDER BY Freq DESC LIMIT 20", data.frame(x=idx[[1]]))
      predict <- findPredict(db, v)
      #print(paste("pred1:", tokens, rev(idx), collapse = " "))
  }
  predict
}

print("Start Shiny Server")
shinyServer(function(input, output, session) {
  db <- dbConnect(SQLite(), "freqTable.db", flags=SQLITE_RO)
  session$onSessionEnded(function() {
    dbDisconnect(db)
    #print("Cleanup session")
  })
  prediction <- reactive({
    st <- proc.time()
    sentences <- lineToSentences(input$text)
    predict = data.frame(Freq=0,Predict=-1,PredictWord="<no prediction>", stringsAsFactors = FALSE)
    scnt <- length(sentences)
    if (scnt >= 1) {
      # Tokenize last sentence, since prediction is for after this...
      v <- tokenizeSentence(sentences[scnt])
      # Split by spaces
      v <- strsplit(v, " ", fixed=TRUE)[[1]]
      # Run prediction
      if (length(v) >= 1) {
        predict <- doPrediction(v, db)
      }
    }
    st <- proc.time() - st
    list(predict, st, mem_used())
  })     
  output$Prediction <- renderText({
    p <- prediction()[[1]]
    #print(p)
    p$PredictWord[[1]]
  })
  output$Prediction2 <- renderText({
    p <- prediction()[[1]]
    ifelse(nrow(p) >= 2, p$PredictWord[[2]], "")
  })
  output$Prediction3 <- renderText({
    p <- prediction()[[1]]
    ifelse(nrow(p) >= 3, p$PredictWord[[3]], "")
  })
  outputOptions(output, "Prediction2", suspendWhenHidden=FALSE)    
  outputOptions(output, "Prediction3", suspendWhenHidden=FALSE)    
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$WordCloud <- renderPlot({
    p <- prediction()[[1]]
    if (is.null(p) || (p$Predict[[1]] < 0)) {
      NULL
    } else {
      wordcloud_rep(p$PredictWord, p$Freq, colors=brewer.pal(8, "Dark2"), max.words=20, min.freq=1, rot.per = 0.3, random.color=TRUE)
    }
  })
  output$ExecTime <- renderText({
    v <- prediction()[[2]]
    paste("User:",format(v[[1]], nsmall=3), "sec, System:", format(v[[2]],nsmall=3), "sec, Total:", format(v[[3]],nsmall=3), "sec")
  })
  output$MemUsage <- renderText({
    mem <- prediction()[[3]]
    paste(format(mem / 1024 / 1024, nsmall=3), "MB")
  })
})
print("Shiny Server Started")