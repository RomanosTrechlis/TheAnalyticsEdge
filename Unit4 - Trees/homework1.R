setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit4 - Trees")

gerber = read.csv("data\\gerber.csv")

# Problem 1.1 - Exploration and Logistic Regression
str(gerber)
table(gerber$voting)
108696/(108696+235388)

# Problem 1.2 - Exploration and Logistic Regression
gerberNumOfRows = nrow(gerber)
sum(subset(gerber, hawthorne==1)$voting)/gerberNumOfRows
sum(subset(gerber, civicduty==1)$voting)/gerberNumOfRows
sum(subset(gerber, neighbors==1)$voting)/gerberNumOfRows
sum(subset(gerber, self==1)$voting)/gerberNumOfRows

# Problem 1.3 - Exploration and Logistic Regression
logreg = glm(voting ~ hawthorne + civicduty + neighbors + self, data=gerber, family="binomial")
summary(logreg)

# Problem 1.4 - Exploration and Logistic Regression
predlog = predict(logreg, type="response")
table(gerber$voting, predlog >= 0.3)
(51966+134513)/(51966+134513+100875+56730)

# Problem 1.5 - Exploration and Logistic Regression
table(gerber$voting, predlog >= 0.5)
235388/(235388+108696)

# Problem 1.6 - Exploration and Logistic Regression
library(ROCR)
ROCRpred_1 = prediction(predlog, gerber$voting)
auc = as.numeric(performance(ROCRpred_1, "auc")@y.values)
auc
table(gerber$voting)
235388/(235388+108696) # baseline accuracy

# Problem 2.1 - Trees
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# Problem 2.2 - Trees
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# Problem 2.4 - Trees
CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)
table(subset(gerber, control==1)$voting, subset(gerber, control==1)$sex)

# Problem 3.1 - Interaction Terms
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits=6)
pred4 = predict(CARTmodel4)
gerber$pred4 = data.frame(pred4)
control = subset(gerber, control==1 & voting==1)
nocontrol = subset(gerber, control==0 & voting==1)
abs(sum(control$pred4)/nrow(control) - sum(nocontrol$pred4)/nrow(nocontrol))

# Problem 3.2 - Interaction Terms
CARTmodel5 = rpart(voting ~ sex + control, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6)

# Problem 3.3 - Interaction Terms
logreg2 = glm(voting ~ sex + control, data=gerber, family="binomial")
summary(logreg2)

# Problem 3.4 - Interaction Terms
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logreg2, newdata=Possibilities, type="response")
abs(0.290456 - 0.2908065)

# Problem 3.5 - Interaction Terms
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

# Problem 3.6 - Interaction Terms
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.290456 - 0.2904558)
