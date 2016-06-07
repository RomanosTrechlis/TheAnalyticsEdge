setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit7 - Visualization")

library(maps)
library(ggplot2)
library(ggmap)

#Problem 1.1 - Drawing a Map of the US
statesMap = map_data("state")
str(statesMap)
summary(statesMap$group)
table(statesMap$group)

# Problem 1.2 - Drawing a Map of the US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")

# Problem 2.1 - Coloring the States by Predictions
polling = read.csv("data\\PollingImputed.csv")
str(polling)
Train = subset(polling, Year >= 2004 & Year <= 2008)
Test = subset(polling, Year >= 2012)
mod2 = glm(Republican ~ SurveyUSA + DiffCount, 
           data=Train, 
           family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(predictionDataFrame$TestPredictionBinary)
mean(predictionDataFrame$TestPrediction)

# Problem 2.2 - Coloring the States by Predictions
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
nrow(predictionMap)
nrow(statesMap)

# Problem 2.4 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black")

# Problem 2.5 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
table(predictionMap$TestPrediction)

# Problem 3.2 - Understanding the Predictions
subset(predictionMap, region == 'florida')$TestPrediction

# PROBLEM 4 - PARAMETER SETTINGS
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", linetype=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", size=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Problem 4.2 - Parameter Settings
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", alpha=0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
