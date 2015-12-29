#
# Do basic data exploration on original source data
#
source("./DownloadAndMakeSentences.R")

# Next, load the data sets
blogs <- readLines("./data/final/en_US/en_US.blogs.txt", encoding="UTF-8")
news <- readLines("./data/final/en_US/en_US.news.txt", encoding="UTF-8")
tweets <- readLines("./data/final/en_US/en_US.twitter.txt", encoding="UTF-8", skipNul=TRUE)

# Display statistics for these data
paste("Blog articles loaded:", length(blogs))
paste("News articles loaded:", length(news))
paste("Tweets loaded:", length(tweets))

# Now, load our sentence parsed data - more useful for us
blogSentence <- readLines("./data/en_US.blogs.sentences.txt.gz", encoding="UTF-8")
newsSentence <- readLines("./data/en_US.news.sentences.txt.gz", encoding="UTF-8")
tweetSentence <- readLines("./data/en_US.twitter.sentences.txt.gz", encoding="UTF-8")

# Display statistics for these data
paste("Blog sentences loaded:", length(blogSentence))
paste("News sentences loaded:", length(newsSentence))
paste("Tweet sentences loaded:", length(tweetSentence))
