library(randomForest)
library(caret)
employee_data <- read.csv("Employee_Data_Project.csv")
employee_data$Attrition <- as.factor(employee_data$Attrition)
install.packages("rpart.plot")
install.packages("mvrsquared")
library(rpart.plot)
library(mvrsquared)
library(dplyr)
library(tidyverse)
set.seed(42)
employee_data = drop_na(employee_data)
train_index <- createDataPartition(employee_data$Attrition, p = 0.7, list = FALSE)
train_data <- employee_data[train_index, ]
test_data <- employee_data[-train_index, ]
model <- train(
  Attrition ~ .,
  data = train_data,
  method = "rf",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE
  ),
  importance = TRUE
)
metric = "ROC"
classProbs = TRUE
summaryFunction = twoClassSummary
print(model)
var_imp <- varImp(model)
print(var_imp)
model_bagged <- train(
  Attrition ~ .,
  data = train_data,
  method = "treebag",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE
  ),
  importance = TRUE
)
metric = "ROC"
classProbs = TRUE
summaryFunction = twoClassSummary
var_imp_bagged <- varImp(model_bagged)
print(var_imp_bagged)
model_bagged
model$finalModel
model_bagged$finalModel
model_bagged$resample
preds_bagged <- predict(model_bagged, test_data, type = "prob")
preds_rf <- predict(model, test_data, type = "prob")
actual_labels <- test_data$Attrition
library(pROC)
roc_bagged <- roc(
  actual_labels,
  preds_bagged$Yes, 
  levels = c("No", "Yes"),
  direction = "<"
)
roc_rf <- roc(
  actual_labels,
  preds_rf$Yes,
  levels = c("No", "Yes"),
  direction = "<"
)
plot(roc_bagged, col = "blue", main = "ROC Curves for Bagged and RF Models")
lines(roc_rf, col = "red")
legend("bottomright", legend = c("Bagged Model", "Random Forest Model"),
       col = c("blue", "red"), lty = 1)
roc_bagged$auc
roc_rf$auc
importance_bagged <- varImp(model_bagged)
print(importance_bagged)
importance_rf <- varImp(model)
print(importance_rf)
plot(importance_bagged, main = "Variable Importance - Bagged Model")
plot(importance_rf, main = "Variable Importance - Random Forest Model")

