# Preparing the Dataset
loans = read.csv("loans.csv")
str(loans)
table(loans$not.fully.paid)
1533/(1533+8045)
summary(loans)
table(subset(loans, !is.na(revol.util))$not.fully.paid)
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

# Prediction Models
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
model1 = glm(not.fully.paid ~ .,data=train, family="binomial")
summary(model1)
logitAminusB = -9.406e-03*700 - -9.406e-03*710
OddAoverOddB = exp(-9.406e-03*700)/exp(-9.406e-03*710)
pred1 = predict(model1, newdata=test, type="response")
test$predicted.risk = data.frame(pred1)$pred1
table(test$not.fully.paid, pred1 > 0.5)
table(test$not.fully.paid)
library(ROCR)
ROCRpredTest = prediction(pred1, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

#  A "Smart Baseline"
model2 = glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(model2)
vars = c("credit.policy","int.rate","installment","log.annual.inc","dti","fico","days.with.cr.line","revol.bal","revol.util","inq.last.6mths","delinq.2yrs","pub.rec","not.fully.paid")
cor(train[vars])
pred2 = predict(model2, newdata=test, type="response")
max(pred2)
table(test$not.fully.paid, pred2 > 0.5)
ROCRpredTest2 = prediction(pred2, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest2, "auc")@y.values)

# Computing the Profitability of an Investment
## investement.pays.back = c[investment] * exp(r[rate]*t[time]) dollars
10*exp(0.06*3) # 10 dollars with interest rate 6% over a period of 3 years
## profit is given by: c * (exp(rt) - 1)

# A Simple Investment Strategy
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10
sum(test$profit)

# An Investment Strategy Based on Risk
highIntRates = subset(test, int.rate >= 0.15)
mean(highIntRates$profit)
table(highIntRates$not.fully.paid)
cutoff = sort(highIntRates$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highIntRates, predicted.risk < cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)


