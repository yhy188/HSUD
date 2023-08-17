library(readxl)
pima_indians_diabetes <- read_excel("C:/Users/11988/Desktop/Êý¾Ý/pima-indians-diabetes.xlsx")
View(pima_indians_diabetes)
dim(pima_indians_diabetes)

library(DMwR)
pima_indians_diabetes$Target<-as.factor(pima_indians_diabetes$Target)
data1<-SMOTE(Target~.,as.data.frame(pima_indians_diabetes),perc.over=820,perc.under=920)
data1$Target<-as.numeric(data1$Target)
prop.table(table(data1$Target))
table(data1$Target)
outcome = data1$Target
data = subset(data1, select = -Target)
str(data)
dim(data)
set.seed(1)
train_obs = sample(nrow(data), 6641)
x_train = data[train_obs, ]
x_holdout = data[-train_obs, ]
outcome_bin = as.numeric(outcome>1)
y_train = outcome_bin[train_obs]
y_holdout = outcome_bin[-train_obs]
table(y_train, useNA = "ifany")
table(y_holdout, useNA = "ifany")
library(SuperLearner)
listWrappers()
SL.glmnet
SL.ranger
SL.xgboost
SL.ksvm
SL.caret
set.seed(1)
sl_lasso = SuperLearner(Y = y_train, X = x_train, family = binomial(),
                        SL.library = "SL.glmnet")

sl_lasso
predictions <- predict.SuperLearner(sl_lasso, newdata=x_holdout)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_holdout=as.factor(y_holdout)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_holdout)
cm


pred = predict(sl_lasso, x_holdout, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_holdout, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

set.seed(1)
sl = SuperLearner(Y = y_train, X = x_train, family = binomial(),cvControl = list(V = 10),
                  SL.library = c( "SL.ranger"
                  ))
sl

predictions <- predict.SuperLearner(sl, newdata=x_holdout)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_holdout=as.factor(y_holdout)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_holdout)
cm


pred = predict(sl, x_holdout, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_holdout, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc



set.seed(1)
sl = SuperLearner(Y = y_train, X = x_train, family = binomial(),cvControl = list(V = 10),
                  SL.library = c("SL.xgboost"
                  ))
sl

predictions <- predict.SuperLearner(sl, newdata=x_holdout)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_holdout=as.factor(y_holdout)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_holdout)
cm


pred = predict(sl, x_holdout, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_holdout, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc


set.seed(1)
sl = SuperLearner(Y = y_train, X = x_train, family = binomial(),cvControl = list(V = 10),
                  SL.library = c("SL.ksvm" ))
sl

predictions <- predict.SuperLearner(sl, newdata=x_holdout)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_holdout=as.factor(y_holdout)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_holdout)
cm


pred = predict(sl, x_holdout, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_holdout, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

set.seed(1)
sl = SuperLearner(Y = y_train, X = x_train, family = binomial(),cvControl = list(V = 10),
                  SL.library = c( "SL.caret"))
sl

predictions <- predict.SuperLearner(sl, newdata=x_holdout)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_holdout=as.factor(y_holdout)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_holdout)
cm


pred = predict(sl, x_holdout, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_holdout, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

library(DMwR)
pima_indians_diabetes$Target<-as.factor(pima_indians_diabetes$Target)
data1<-SMOTE(Target~.,as.data.frame(pima_indians_diabetes),perc.over=820,perc.under=920)
data1$Target<-as.numeric(data1$Target)
prop.table(table(data1$Target))
table(data1$Target)
outcome = data1$Target
data = subset(data1, select = -Target)
str(data)
dim(data)
set.seed(1)
train_obs = sample(nrow(data), 6641)
x_train = data[train_obs, ]
x_holdout = data[-train_obs, ]
outcome_bin = as.numeric(outcome>1)
y_train = outcome_bin[train_obs]
y_holdout = outcome_bin[-train_obs]
table(y_train, useNA = "ifany")
table(y_holdout, useNA = "ifany")
library(SuperLearner)
listWrappers()
SL.glmnet
SL.ranger
SL.xgboost
SL.ksvm
SL.caret

set.seed(1)
sl = SuperLearner(Y = y_holdout, X = x_holdout, family = binomial(),cvControl = list(V = 10),
                  SL.library = c("SL.glmnet"
                  ))

sl

predictions <- predict.SuperLearner(sl, newdata=x_train)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_train=as.factor(y_train)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_train)
cm

pred = predict(sl, x_train, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_train, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_train)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

set.seed(1)
sl = SuperLearner(Y = y_holdout, X = x_holdout, family = binomial(),cvControl = list(V = 10),
                  SL.library = c("SL.ranger"
                  ))

sl

predictions <- predict.SuperLearner(sl, newdata=x_train)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_train=as.factor(y_train)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_train)
cm

pred = predict(sl, x_train, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_train, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_train)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

set.seed(1)
sl = SuperLearner(Y = y_holdout, X = x_holdout, family = binomial(),cvControl = list(V = 10),
                  SL.library = c( "SL.xgboost" ))

sl

predictions <- predict.SuperLearner(sl, newdata=x_train)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_train=as.factor(y_train)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_train)
cm

pred = predict(sl, x_train, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_train, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_train)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

set.seed(1)
sl = SuperLearner(Y = y_holdout, X = x_holdout, family = binomial(),cvControl = list(V = 10),
                  SL.library = c("SL.ksvm"  ))

sl

predictions <- predict.SuperLearner(sl, newdata=x_train)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_train=as.factor(y_train)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_train)
cm

pred = predict(sl, x_train, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_train, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_train)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc


set.seed(1)
sl = SuperLearner(Y = y_holdout, X = x_holdout, family = binomial(),cvControl = list(V = 10),
                  SL.library = c("SL.caret" ))

sl

predictions <- predict.SuperLearner(sl, newdata=x_train)
head(predictions$pred)
head(predictions$library.predict)
library(dplyr)
conv.preds <- ifelse(predictions$pred>=0.5, 1, 0)
conv.preds=as.factor(conv.preds)
y_train=as.factor(y_train)
library(e1071)
library(caret)
cm<- confusionMatrix(conv.preds,y_train)
cm

pred = predict(sl, x_train, onlySL = TRUE)
str(pred)
summary(pred$library.predict)
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()
qplot(y_train, pred$pred[, 1]) + theme_minimal()
pred_rocr = ROCR::prediction(pred$pred, y_train)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc