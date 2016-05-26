setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit2 - Linear Regression")

wine = read.csv("data\\wine.csv")
summary(wine)
str(wine)
model1 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model1)

cor(wine$HarvestRain, wine$WinterRain)