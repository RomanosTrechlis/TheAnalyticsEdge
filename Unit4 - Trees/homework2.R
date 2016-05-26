setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit4 - Trees")

letters = read.csv("data\\letters_ABPR.csv")

# Problem 1.1 - Predicting B or not B
letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
Train = subset(letters, spl==TRUE)
Test = subset(letters, spl==FALSE)
table(letters$isB)
2350/(2350+766)

# Problem 1.2 - Predicting B or not B
CARTb = rpart(isB ~ . - letter, data=Train, method="class")
prp(CARTb)
pred = predict(CARTb, newdata=Test, type="class")
table(Test$isB, pred)
(1118+340)/(1118+340+57+43)

# Problem 1.3 - Predicting B or Not B
library(randomForest)
set.seed(1000)
forest = randomForest(isB ~ . - letter, data=Train)
PredictForest = predict(forest, newdata = Test)
table(Test$isB, PredictForest)
(1165+374)/(1165+374+9+10)

# Problem 2.1 - Predicting the letters A, B, P, R
letters$letter = as.factor(letters$letter)
set.seed(2000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
Train = subset(letters, spl==TRUE)
Test = subset(letters, spl==FALSE)
table(letters$letter)
803/(803+789+766+758)

# Problem 2.2 - Predicting the letters A, B, P, R
CARTMulti = rpart(letter ~ . - isB, data=Train, method="class")
prp(CARTMulti)
predMulti = predict(CARTMulti, newdata=Test, type="class")
table(Test$letter, predMulti)
(345+317+360+332)/nrow(Test)

# Problem 2.3 - Predicting the letters A, B, P, R
set.seed(1000)
forestMulti = randomForest(letter ~ . - isB, data=Train)
PredictForestMulti = predict(forestMulti, newdata = Test)
table(Test$letter, PredictForestMulti)
(392+379+389+372)/nrow(Test)

