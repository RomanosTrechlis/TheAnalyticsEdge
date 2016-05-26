setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit4 - Trees")

census = read.csv("data\\census.csv")

# Problem 1.1 - A Logistic Regression Model
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
Train = subset(census, spl==TRUE)
Test = subset(census, spl==FALSE)
model1 = glm(over50k ~ . , data=Train, family="binomial")
summary(model1)

# Problem 1.2 - A Logistic Regression Model
pred1 = predict(model1, newdata=Test, type="response")
table(Test$over50k, pred1 >= 0.5)
(9051+1888)/(9051+1888+662+1190)

# Problem 1.3 - A Logistic Regression Model
table(Test$over50k)
9713/(9713+3078)

# Problem 1.4 - A Logistic Regression Model
library(ROCR)
ROCRpred1 = prediction(pred1, Test$over50k)
auc = as.numeric(performance(ROCRpred1, "auc")@y.values)
auc

# Problem 2.1 - A CART Model
model2 = rpart(over50k ~ ., data=Train, method="class")
prp(model2)

# Problem 2.4 - A CART Model
pred2 = predict(model2, newdata=Test, type="class")
table(Test$over50k, pred2)
(9243+1596)/(9243+1596+470+1482)

# Problem 2.6 - A CART Model
pred2 = predict(model2, newdata=Test)
library(ROCR)
ROCRpred2 = prediction(pred2[,2], Test$over50k)
auc = as.numeric(performance(ROCRpred2, "auc")@y.values)
auc
ROCRperf = performance(ROCRpred2, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

# Problem 3.1 - A Random Forest Model
set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]
model3 = randomForest(over50k ~ ., data=trainSmall)
pred3 = predict(model3, newdata=Test)
table(Test$over50k, pred3)
(9614+1050)/(9614+1050+99+2028)

# Problem 3.2 - A Random Forest Model
vu = varUsed(model3, count=TRUE)
vu
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(model3$forest$xlevels[vusorted$ix]))

# Problem 3.3 - A Random Forest Model
varImpPlot(model3)

# Problem 4.1 - Selecting cp by Cross-Validation
set.seed(2)
library(caret)
library(e1071)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data = Train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
model4 = rpart(over50k ~ ., data = Train, method="class", cp = 0.002)
pred4 = predict(model4, newdata = Test, type = "class")
table(Test$over50k, pred4)
(9178+1838)/(9178+1838+535+1240)
prp(model4)





