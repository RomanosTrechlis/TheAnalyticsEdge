setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit2")

data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

# DATA EXPLORATION
str(statedata)

plot(statedata$x, statedata$y)

tapply(statedata$HS.Grad, statedata$state.region, mean)

tapply(statedata$Murder, statedata$state.region, median)
boxplot(Murder ~ state.region, data=statedata)

subset(statedata, state.region == 'Northeast')

# PREDICTING LIFE EXPECTANCY: INITIAL MODEL
model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(model1)

plot(statedata$Income, statedata$Life.Exp)


# PREDICTING LIFE EXPECTANCY: REFINING THE MODEL
## REMOVING AREA VARIABLE
model2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data=statedata)
summary(model2)
## REMOVING ILLITERACY VARIABLE
model3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data=statedata)
summary(model3)
## REMOVING INCOME VARIABLE
model4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(model4)
pred = predict(model4)
statedata$state.name[which.min(statedata$Life.Exp)]
statedata$state.name[which.max(statedata$Life.Exp)]
sort(abs(model$residuals))




