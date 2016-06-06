setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit6 - Clustering")

# Problem 1.1 - Exploring the Dataset
stocks = read.csv("data\\StocksCluster.csv")
str(stocks)

# Problem 1.2 - Exploring the Dataset
nrow(subset(stocks, PositiveDec > 0))
6324/11580

# Problem 1.3 - Exploring the Dataset
cor(stocks)
0.19167279

# Problem 1.4 - Exploring the Dataset
summary(stocks)
0.026308

# Problem 2.1 - Initial Logistic Regression Model
set.seed(144)
library(caTools)
library(e1071)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family="binomial")
StocksPredict = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, StocksPredict >= 0.5)
(990 + 3640)/(990 + 3640 + 2689 + 787)

# Problem 2.2 - Initial Logistic Regression Model
StocksPredict = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, StocksPredict >= 0.5)
(417 + 1553)/(417 + 1553 + 1160 + 344)

# Problem 2.3 - Initial Logistic Regression Model
table(stocksTest$PositiveDec)
1897/(1897 + 1577)

# Problem 3.1 - Clustering Stocks
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# Problem 3.2 - Clustering Stocks
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain)
summary(normTest)

# Problem 3.4 - Clustering Stocks
set.seed(144)
KMC = kmeans(normTrain, centers = 3)
clusters = KMC$cluster
cluster1 = subset(normTrain, clusters == 1)
cluster2 = subset(normTrain, clusters == 2)
cluster3 = subset(normTrain, clusters == 3)
nrow(cluster3)

# Problem 3.5 - Clustering Stocks
library(flexclust)
KMC.kcca = as.kcca(KMC, normTrain)
clusterTrain = predict(KMC.kcca)
clusterTest = predict(KMC.kcca, newdata=normTest)
nrow(subset(normTest, clusterTest == 2))

# Problem 4.1 - Cluster-Specific Predictions
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
summary(stocksTrain3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain3$PositiveDec)

# Problem 4.2 - Cluster-Specific Predictions
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family="binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family="binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family="binomial")
StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients

# Problem 4.3 - Cluster-Specific Predictions
predictTest1 = predict(StocksModel1, newdata=stocksTest1, type = "response")
predictTest2 = predict(StocksModel2, newdata=stocksTest2, type = "response")
predictTest3 = predict(StocksModel3, newdata=stocksTest3, type = "response")
table(stocksTest1$PositiveDec, predictTest1 >= 0.5)
(774 + 30)/(774 + 30 + 23 + 471)
table(stocksTest2$PositiveDec, predictTest2 >= 0.5)
(388 + 757)/(388 + 757 + 626 + 309)
table(stocksTest3$PositiveDec, predictTest3 >= 0.5)
(49 + 13)/(49 + 13 + 13 + 21)

# Problem 4.4 - Cluster-Specific Predictions
AllPredictions = c(predictTest1, predictTest2, predictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions >= 0.5)
(467 + 1544)/(467 + 1544 + 1110 + 353)

