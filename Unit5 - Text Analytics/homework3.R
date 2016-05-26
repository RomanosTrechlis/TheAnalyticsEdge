setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit5 - Text Analytics")

# Problem 1.1 - Loading the Data
emails = read.csv("data\\emails.csv", stringsAsFactors=FALSE)
nrow(emails)

# Problem 1.2 - Loading the Dataset
str(emails)
sum(emails$spam)

# Problem 1.5 - Loading the Dataset
max(nchar(emails$text))

# Problem 1.5 - Loading the Dataset
which.min(nchar(emails$text))

# Problem 2.1 - Preparing the Corpus
library(tm)
library(SnowballC)
corpus  = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

# Problem 2.2 - Preparing the Corpus
spdtm  = removeSparseTerms(dtm, 0.95)
spdtm

# Problem 2.3 - Preparing the Corpus
emailsSparse  = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

# Problem 2.4 - Preparing the Corpus
emailsSparse$spam = emails$spam
sort(colSums(subset(emailsSparse, spam == 0)))

# Problem 2.5 - Preparing the Corpus
sort(colSums(subset(emailsSparse, spam == 1)))

# Problem 3.1 - Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)
table(emailsSparse$spam)
spamLog = glm(spam ~ ., data=train, family="binomial")
summary(spamLog)
spamCART = rpart(spam ~ ., data=train, method="class")
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)
logPred = predict(spamLog, type="response")
str(logPred)
pred.log = logPred
treePred = predict(spamCART)
pred.tree = treePred[,2]
forestPred = predict(spamRF, type="prob")
pred.forest = forestPred[,2]
sum(pred.log < 0.00001)
sum(pred.log > 0.99999)
sum(pred.log <= 0.99999 & pred.log >= 0.00001)

# Problem 3.2 - Building Machine Learning Models
prp(spamCART)

# Problem 3.4 - Building Machine Learning Models
table(train$spam, pred.log >= 0.5)
(3052+954)/(3052+954+4+0)

# Problem 3.5 - Building Machine Learning Models
library(ROCR)
predROCR = prediction(pred.log, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

# Problem 3.6 - Building Machine Learning Models
table(train$spam, pred.tree >= 0.5)
(2885+894)/(2885+894+167+64)

# Problem 3.7 - Building Machine Learning Models
predROCR = prediction(pred.tree, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

# Problem 3.8 - Building Machine Learning Models
table(train$spam, pred.forest >= 0.5)
(3013+914)/(3013+914+39+44)

# Problem 3.9 - Building Machine Learning Models
predROCR = prediction(pred.forest, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

# Problem 4.1 - Evaluating on the Test Set
logPred = predict(spamLog, type="response", newdata=test)
predTest.log = logPred
treePred = predict(spamCART, newdata=test)
predTest.tree = treePred[,2]
forestPred = predict(spamRF, type="prob", newdata=test)
predTest.forest = forestPred[,2]
table(test$spam, predTest.log >= 0.5)
(1257+376)/(1257+376+51+34)

# Problem 4.2 - Evaluating on the Test Set
predROCR = prediction(predTest.log, test$spam)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

# Problem 4.3 - Evaluating on the Test Set
table(test$spam, predTest.tree >= 0.5)
(1228+386)/(1228+386+80+24)

# Problem 4.4 - Evaluating on the Test Set
predROCR = prediction(predTest.tree, test$spam)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

# Problem 4.5 - Evaluating on the Test Set
table(test$spam, predTest.forest >= 0.5)
(1290+386)/(1290+386+18+24)

# Problem 4.6 - Evaluating on the Test Set
predROCR = prediction(predTest.forest, test$spam)
auc.perf = performance(predROCR, measure = "auc")
auc.perf@y.values

