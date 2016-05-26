# Loading the Dataset
setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit3 - Logistic Regression")
data = read.csv("data\\parole.csv")
str(data)
table(data$violator)
table(data$multiple.offenses)

# Preparing the Dataset
data$crime = as.factor(data$crime)
data$state = as.factor(data$state)
str(data)
summary(data)

# Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split = sample.split(data$violator, SplitRatio = 0.7)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)
str(train)

# Building a Logistic Regression Model
model1 = glm(violator ~ ., data=train, family="binomial")
summary(model1)
logit = -4.2411574 + 0.3869904 + 0.8867192 + -0.0001756*50 + -0.1238867*3 + 0.0802954*12 + 0.6837143
odds=exp(logit)
probability = 1 / (1 + exp(-logit))

# Evaluating the Model on the Testing Set
pred1 = predict(model1, newdata=test, type="response")
max(pred1)
table(test$violator, pred1 >= 0.5)
sensitivity = 12/(12+11) # the 1 ones
specificity = 167/(167+12) # the 0 ones
accuracy = (167+12)/(167+12+12+11)
table(test$violator)
179/(179+23)
table(test$violator, pred1 >= 0.7)
# If the board used the model for parole decisions, 
# a negative prediction would lead to a prisoner being 
# granted parole, while a positive prediction would lead to 
# a prisoner being denied parole. The parole board would 
# experience more regret for releasing a prisoner who then 
# violates parole (a negative prediction that is actually 
# positive, or false negative) than it would experience 
# for denying parole to a prisoner who would not have violated 
# parole (a positive prediction that is actually negative, or false positive).
# 
# Decreasing the cutoff leads to more positive predictions, 
# which increases false positives and decreases false negatives. 
# Meanwhile, increasing the cutoff leads to more negative predictions, 
# which increases false negatives and decreases false positives. 
# The parole board assigns high cost to false negatives, and therefore 
# should decrease the cutoff.  
library(ROCR)
ROCRpredTest = prediction(pred1, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)




