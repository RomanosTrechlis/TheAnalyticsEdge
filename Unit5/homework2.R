setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit5")

# Problem 1.1 - Loading the Data
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
max(nchar(trials$abstract))

# Problem 1.2 - Loading the Data
nrow(subset(trials, abstract == ""))

# Problem 1.3 - Loading the Data
min(nchar(trials$title))
subset(trials, nchar(title)==28)$title

# Problem 2.1 - Preparing the Corpus
library(tm)
library(SnowballC)
corpusTitle  = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
str(dtmTitle)
str(dtmAbstract)

# Problem 2.3 - Preparing the Corpus
which.max(colSums(dtmAbstract))

# Problem 3.1 - Building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# Problem 3.2 - Building a Model
dtm = cbind(dtmTitle, dtmAbstract)
str(dtm)
dtm$trial = trials$trial

# Problem 3.3 - Building a Model
set.seed(144)
library(caTools)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)
table(train$trial)
730/(730+572)

# Problem 3.4 - Building a Model
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

# Problem 3.5 - Building a Model
trainPred = predict(trialCART)
str(trainPred)
pred.prob = trainPred[,2]
max(pred.prob)

# Problem 3.7 - Building a Model
table(train$trial, pred.prob >= 0.5)
(631+441)/(631+441+99+131)
(441)/(441+131)
(631)/(631+99)

# Problem 4.1 - Evaluating the model on the testing set
testPred = predict(trialCART, newdata=test)
str(testPred)
pred.prob = data.frame(testPred)$X1
table(test$trial, pred.prob >= 0.5)
(261+162)/(261+162+52+83)

# Problem 4.2 - Evaluating the Model on the Testing Set
library(ROCR)
predROCR = prediction(pred.prob, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

