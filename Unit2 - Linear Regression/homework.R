setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit2 - Linear Regression")

# Climate Change
# ==============
climate = read.csv("data\\climate_change.csv")
summary(climate)
str(climate)

train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model1)
cor(train)

model2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=train)
summary(model2)

stepModel = step(model1)
summary(stepModel)

prediction = predict(stepModel, newdata=test)
summary(prediction)
str(prediction)



SSE = sum((prediction - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST


# PISA STUDENT COMPETITION
# ========================
ls()
rm(climate, model1, model2, prediction, R2, SSE, SST, stepModel, test, train)

train = read.csv("data\\pisa2009train.csv")
test = read.csv("data\\pisa2009test.csv")

str(train)
summary(train)

tapply(train$readingScore, train$male, mean)

train = na.omit(train)
test = na.omit(test)
str(train)

train$raceeth = relevel(train$raceeth, "White")
test$raceeth = relevel(test$raceeth, "White")

lmScore = lm(readingScore ~ ., data=train)

SSE_train = sum(lmScore$residuals^2)
rmse_train = sqrt(SSE_train/nrow(train))

predTest = predict(lmScore, newdata=test)
SSE = sum((predTest - test$readingScore)^2)
SST = sum((mean(train$readingScore) - test$readingScore)^2)
R2 = 1 - SSE/SST
rmse_test = sqrt(SSE/nrow(test))


# Infuenza outbreaks prediction based on google searches
# ======================================================
ls()
rm(test, train, R2, lmScore, predTest, rmse_test, rmse_train, SSE, SST)
fluTrain = read.csv("data\\FluTrain.csv")
hist(fluTrain$ILI)
plot(log(fluTrain$ILI), fluTrain$Queries)

FluTrend = lm(log(ILI) ~ Queries, data=fluTrain)
summary(FluTrend)

R2 = cor(log(fluTrain$ILI), fluTrain$Queries)^2

fluTest = read.csv("data\\FluTest.csv")

PredTest1 = predict(FluTrend, newdata=fluTest)

# However, the dependent variable in our model is log(ILI), 
# so PredTest1 would contain predictions of the log(ILI) value. 
# We are instead interested in obtaining predictions of the ILI value. 
# We can convert from predictions of log(ILI) to predictions of ILI
# via exponentiation, or the exp() function. The new code, 
# which predicts the ILI value, is
PredTest1 = exp(predict(FluTrend, newdata=fluTest))

(fluTest$ILI - PredTest1)/fluTest$ILI
SSE = sum((PredTest1 - fluTest$ILI)^2)
SST = sum((mean(fluTrain$ILI) - fluTest$ILI)^2)
R2 = 1 - SSE/SST
rmse_test = sqrt(SSE/nrow(fluTest))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)

plot(log(fluTrain$ILILag2), log(fluTrain$ILI))

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=fluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad=TRUE)
fluTest$ILILag2 = coredata(ILILag2)

PredTest2 = exp(predict(FluTrend2, newdata=fluTest))
SSE = sum((PredTest2 - fluTest$ILI)^2)
SST = sum((mean(fluTrain$ILI) - fluTest$ILI)^2)
R2 = 1 - SSE/SST
rmse_test = sqrt(SSE/nrow(fluTest))