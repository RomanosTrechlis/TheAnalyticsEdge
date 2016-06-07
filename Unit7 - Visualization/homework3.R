setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit7 - Visualization")

# Problem 1.1 - Preparing the Data
tweets = read.csv("data\\tweets.csv", stringsAsFactors=FALSE)
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet ))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
str(allTweets)

# Problem 2.1 - Building a Word Cloud
library(wordcloud)
?wordcloud

# Problem 2.2 - Building a Word Cloud
colSums(allTweets)

# Problem 2.3 - Building a Word Cloud
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# Problem 2.4 - Building a Word Cloud
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# Problem 3.1 - Size and Color
?wordcloud

# Problem 3.3 - Size and Color
wordcloud(colnames(allTweets), 
          colSums(allTweets), 
          scale=c(2, 0.25),
          min.freq = 10,
          random.order = FALSE)

# Problem 4.1 - Selecting a Color Palette
library(RColorBrewer)
display.brewer.all()

# Problem 4.3 - Selecting a Color Palette
wordcloud(colnames(allTweets), 
          colSums(allTweets), 
          scale=c(2, 0.25),
          min.freq = 10,
          colors=brewer.pal(9, "Blues")[c(1,2,3,4)])



