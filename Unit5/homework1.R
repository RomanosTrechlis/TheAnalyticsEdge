setwd("C:\\Users\\xa86\\Desktop\\Unit5")

# Problem 1.1 - Bags of Words
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

# Problem 1.2 - Bags of Words
library(tm)
library(SnowballC)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# Problem 1.3 - Bags of Words
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Problem 1.4 - Bags of Words
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# Problem 1.5 - Bags of Words
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)
table(test$Vandal)
618/(618+545)

# Problem 1.6 - Bags of Words
library(rpart)
library(rpart.plot)
modelCART = rpart(Vandal ~ ., data=train, method="class")
predCART = predict(modelCART, newdata=test, type="class")
table(test$Vandal, predCART)
(618+12)/(618+12+533)

# Problem 1.7 - Bags of Words
prp(modelCART)

# Problem 2.1 - Problem-specific Knowledge
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)

# Problem 2.2 - Problem-Specific Knowledge
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
modelCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(modelCART2)
predCART2 = predict(modelCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predCART2)
(609+57)/(609+57+9+488)

# Problem 2.3 - Problem-Specific Knowledge
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# Problem 2.4 - Problem-Specific Knowledge
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
modelCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(modelCART2)
predCART2 = predict(modelCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predCART2)
(514+248)/(514+248+104+297)

# Problem 3.1 - Using Non-Textual Data
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
modelCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
predCART3 = predict(modelCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predCART3)
(595+241)/(595+241+23+304)

# Problem 3.2 - Using Non-Textual Data
prp(modelCART3)
