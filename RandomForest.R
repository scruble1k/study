setwd("D:/R_code/RandomForest")

library(tidyverse)
library(caret)
library(randomForest)
library(MASS)

site_group <- read.csv("METRO_ES1.csv")

#test multicolinearity


# make dependent variable as a factor (categorical)
dim(site_group)
site_group$GROUP = as.factor(site_group$GROUP)

set.seed(71)
rf <-randomForest(GROUP~.,data=site_group, ntree=500) 
rf

floor(sqrt(ncol(site_group) - 1))

mtry <- tuneRF(site_group[-1],site_group$GROUP, ntreeTry=500, stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(71)
rf <-randomForest(GROUP~.,data=site_group, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], site_group$GROUP)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


site_group$GROUP = as.factor(site_group$GROUP)

set.seed(10)
rf.fit <-randomForest(GROUP~., data=site_group, ntree=500, mtry = floor(sqrt(18)), importance = T) 
rf.fit

test_x = test.bundle[c("TEM", "RAIN", "POP", "LU_HOU", "LU_IND", "LU_REC", "LU_ROAD", "LU_AG", "LU_FO", "LU_GR", "LU_WA", "ELE", "SLO", "PD", "DI", "SD", "CO")]
test_y = test.bundle$GROUP


y_pred = predict(rf.fit, test_x)
y_pred

confusionMatrix(y_pred, test_y)

importance(rf.fit)
varImpPlot(rf.fit)

set.seed(17)
fgl.rf <- randomForest(GROUP~., data=site_group, mtry=floor(sqrt(18)), importance = T, do.trace = 100)
print(fgl.rf)

library(ipred)
set.seed(131)
error.RF <- numeric(10)
 for(i in 1:10) error.RF[i] <- errorest(GROUP~., data=site_group, model = randomForest, mtry = 2)$error

summary(error.RF)

library(e1071)
set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10) error.SVM[i] <- errorest(GROUP~., data=site_group, model = svm, cost = 10, gamma = 1.5)$error
summary(error.SVM)
