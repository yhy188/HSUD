library(readxl)
xu2_gdm_PrU2_ML <- read_excel("C:/Users/11988/Desktop/Êý¾Ý/xu2_gdm_PrU2_ML.xlsx")
View(xu2_gdm_PrU2_ML)
str(xu2_gdm_PrU2_ML)
dim(xu2_gdm_PrU2_ML)
library(DMwR)
xu2_gdm_PrU2_ML$class<-as.factor(xu2_gdm_PrU2_ML$class)
data1<-SMOTE(class~.,as.data.frame(xu2_gdm_PrU2_ML),perc.over=833,perc.under=933)
data1$class<-as.numeric(data1$class)
prop.table(table(data1$class))
table(data1$class)
outcome = data1$class
outcome=as.numeric(outcome)
data = subset(data1, select = -class)
str(data)
dim(data)
set.seed(1)
train_obs = sample(nrow(data), 10614)
x_train = data[train_obs, ]
x_holdout = data[-train_obs, ]
outcome_bin = as.numeric(outcome>1 )
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

