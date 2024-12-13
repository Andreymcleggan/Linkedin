employee_data <- read.csv("Employee_Data_Project.csv")
employee_data <- na.omit(employee_data)

#load packges
library(tidyverse)
library(ROSE)
library(caret)
library(broom)
library(ggridges)
library(readxl)
library(pROC)
library(psych)
library(stats)

dim(employee_data)
str(employee_data)
anyNA(employee_data)
describe(employee_data)
summary(employee_data)

#change to numerics
employee_data$NumCompaniesWorked = as.numeric(employee_data$NumCompaniesWorked)
employee_data$TotalWorkingYears = as.numeric(employee_data$TotalWorkingYears)
employee_data$EnvironmentSatisfaction = as.numeric(employee_data$EnvironmentSatisfaction)
employee_data$JobSatisfaction = as.numeric(employee_data$JobSatisfaction)

str(employee_data)

# check missing values
employee_data %>%
  summarize(
    across(everything(), function(x) sum(is.na(x)))
  )


## Question 2: 

# Convert Attrition to binary (1 for "Yes", 0 for "No")
employee_data$Attrition_Binary <- ifelse(employee_data$Attrition == "Yes", 1, 0)

# Model 4: Logistic regression with interaction term Gender:Income
model_4 <- glm(Attrition_Binary ~ Age + Gender + JobSatisfaction + Income + Gender:Income,
               data = employee_data, family = binomial)

# Summary of the model
summary(model_4)

# Predict probabilities for combinations of predictors
ages_genders_satisfaction_income <- expand.grid(
  Age = c(25, 40, 60),
  Gender = c("Male", "Female"),
  JobSatisfaction = c(1, 4),  # Example satisfaction levels (low and high)
  Income = c(30000, 70000, 150000)  # Example income levels
)
predicted_probs <- predict(model_4, newdata = ages_genders_satisfaction_income, type = "response")
 
# Combine predictors and predicted probabilities
results_4 <- data.frame(ages_genders_satisfaction_income, Predicted_Probability = predicted_probs)

# Display the results
print(results_4)


## Question 3: 

# Calculate the marginal distribution
marginal_distribution <- prop.table(table(employee_data$Attrition)) * 100

# Convert to a data frame for better visualization
marginal_distribution_report <- data.frame(
  Attrition = names(marginal_distribution),
  Percentage = as.numeric(marginal_distribution)
)

# Display the result
print(marginal_distribution_report)

# Drop rows with missing values
employee_data <- na.omit(employee_data)

# Create a binary variable for age group (Younger: <= 40, Older: > 40)
employee_data$Age_Group <- ifelse(employee_data$Age <= 40, "Younger", "Older")

# Convert Attrition to binary (1 for "Yes", 0 for "No")
employee_data$Attrition_Binary <- ifelse(employee_data$Attrition == "Yes", 1, 0)

# Logistic regression model: Attrition ~ Age_Group
logit_model <- glm(Attrition_Binary ~ Age_Group, data = employee_data, family = "binomial")

# Predict probabilities for younger and older employees
younger_prob <- predict(logit_model, newdata = data.frame(Age_Group = "Younger"), type = "response")
older_prob <- predict(logit_model, newdata = data.frame(Age_Group = "Older"), type = "response")

# Display results
cat("Logistic Regression Results for Age Groups:\n")
cat(sprintf("Probability of Attrition (Younger Employees, Age <= 40): %.2f%%\n", younger_prob * 100))
cat(sprintf("Probability of Attrition (Older Employees, Age > 40): %.2f%%\n", older_prob * 100))


## Question 4: 

# New data --> age for prediction
employee_data$Age_Group_Factor <- ifelse(employee_data$Age <= 40, 1, 0)
logit_model2 <- glm(Attrition_Binary ~ Age, data = employee_data, family = "binomial")
age_new = seq(min(employee_data$Age), max(employee_data$Age), by = 1)
prediction1 = data.frame(Age = age_new)


# Predicted probabilities
prediction1$prediction_prob = predict(logit_model2, newdata = prediction1, type = "response")

# Plot the relationship
log1 = ggplot(employee_data, aes(x = Age, y = Attrition_Binary)) +
  geom_point(alpha = 0.3, color = "pink") +
  geom_line(data = prediction1, aes(x = Age, y = prediction_prob), color = "green", size = 1) +
  labs(title = "Age v Attrition",
       x = "Age",
       y = "Probability of Attrition") +
  theme_minimal()

print(log1)

#Pink dots represent the actual data points of age and attrition. The zeros are people who stayed and ones are people that left.
#Green line is the predicted probability of attrition, according to the model younger employees are more likely to leave (higher probability), and older employees are less likely to leave (lower probability).

# Age Statistics for Attrition Groups
age_summary =  employee_data %>%
  group_by(Attrition) %>%
  summarise(
    count = n(),
    mean_age = mean(Age),
    sd_age = sd(Age),
    min_age = min(Age),
    max_age = max(Age),
    median_age = median(Age)
  )

print(age_summary)
# The table shows that younger Canterra employees are more likely to leave the company because there is a lower mean and median age in the leaving group. The employees that decide to stay have a higher average age and lower variation in age range.


## Question 5: 

# Stratified split for training (60%), validation (20%), and test (20%)
set.seed(123)  # For reproducibility

# First, split into training and temporary (validation + test) sets
train_index <- createDataPartition(employee_data$Attrition_Binary, p = 0.6, list = FALSE)
train_set <- employee_data[train_index, ]
temp_set <- employee_data[-train_index, ]

# Now, split the temporary set into validation and test sets (50-50 split of remaining 40%)
validation_index <- createDataPartition(temp_set$Attrition_Binary, p = 0.5, list = FALSE)
validation_set <- temp_set[validation_index, ]
test_set <- temp_set[-validation_index, ]

# Model 1: A one-variable model with Age
model_1 <- glm(Attrition_Binary ~ Age, data = train_set, family = "binomial")

# Model 2: A two-variable model with Age and Gender
model_2 <- glm(Attrition_Binary ~ Age + Gender, data = train_set, family = "binomial")

# Model 3: A three-variable model with Age, Gender, and JobSatisfaction
model_3 <- glm(Attrition_Binary ~ Age + Gender + JobSatisfaction, data = train_set, family = "binomial")

# Model 4: An interaction model with Age, Gender, JobSatisfaction, Income, and Gender:Income
model_4 <- glm(Attrition_Binary ~ Age + Gender + JobSatisfaction + Income + Gender:Income,
               data = train_set, family = "binomial")

# Summarize each model
cat("\nSummary of Model 1:\n")
summary(model_1)

cat("\nSummary of Model 2:\n")
summary(model_2)

cat("\nSummary of Model 3:\n")
summary(model_3)

cat("\nSummary of Model 4:\n")
summary(model_4)

# Rebalance the training set using oversampling and undersampling
balanced_train_set <- ovun.sample(Attrition_Binary ~ ., data = train_set, method = "both")$data

# Rebalance the validation set using oversampling
balanced_validation_set <- ovun.sample(Attrition_Binary ~ ., data = validation_set, method = "over")$data

# Retrain all models on the rebalanced training set
model_1_balanced <- glm(Attrition_Binary ~ Age, data = balanced_train_set, family = "binomial")
model_2_balanced <- glm(Attrition_Binary ~ Age + Gender, data = balanced_train_set, family = "binomial")
model_3_balanced <- glm(Attrition_Binary ~ Age + Gender + JobSatisfaction, data = balanced_train_set, family = "binomial")
model_4_balanced <- glm(Attrition_Binary ~ Age + Gender + JobSatisfaction + Income + Gender:Income,
                        data = balanced_train_set, family = "binomial")

# Function to evaluate a model
evaluate_model <- function(model, validation_data) {
  # Predict probabilities and binary outcomes
  predicted_probs <- predict(model, newdata = validation_data, type = "response")
  predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
  
  # Ensure factor levels match for predicted and actual classes
  predicted_classes <- factor(predicted_classes, levels = c(0, 1))
  actual_classes <- factor(validation_data$Attrition_Binary, levels = c(0, 1))
  
  # Calculate AIC
  aic <- AIC(model)
  
  # Calculate AUC
  roc_curve <- roc(actual_classes, predicted_probs)
  auc <- auc(roc_curve)
  
  # Calculate Precision and Recall
  confusion <- confusionMatrix(predicted_classes, actual_classes, positive = "1")
  precision <- confusion$byClass["Precision"]
  recall <- confusion$byClass["Recall"]
  
  return(c(AIC = aic, AUC = auc, Precision = precision, Recall = recall))
}

# Evaluate all models on the balanced validation set
results <- rbind(
  Model_1 = evaluate_model(model_1_balanced, balanced_validation_set),
  Model_2 = evaluate_model(model_2_balanced, balanced_validation_set),
  Model_3 = evaluate_model(model_3_balanced, balanced_validation_set),
  Model_4 = evaluate_model(model_4_balanced, balanced_validation_set)
)

# Convert results to a data frame
results_df <- as.data.frame(results)
results_df$Model <- rownames(results_df)
rownames(results_df) <- NULL

# Print the results table
print(results_df)


## Question 6: 

pred_1 <- predict(model_1_balanced, newdata = test_set, type = "response")
pred_2 <- predict(model_2_balanced, newdata = test_set, type = "response")
pred_3 <- predict(model_3_balanced, newdata = test_set, type = "response")
pred_4 <- predict(model_4_balanced, newdata = test_set, type = "response")


#ROC Curves and AUC
roc_1 <- roc(test_set$Attrition_Binary, pred_1)
roc_2 <- roc(test_set$Attrition_Binary, pred_2)
roc_3 <- roc(test_set$Attrition_Binary, pred_3)
roc_4 <- roc(test_set$Attrition_Binary, pred_4)


# ROC curves on one plot
plot(roc_1, col = "blue", main = "ROC Curves for Models 1-4", lwd = 2)
plot(roc_2, col = "red", add = TRUE, lwd = 2)
plot(roc_3, col = "green", add = TRUE, lwd = 2)
plot(roc_4, col = "purple", add = TRUE, lwd = 2)

# Legend
legend("bottomright", legend = c("Model 1", "Model 2", "Model 3", "Model 4"),
       col = c("blue", "red", "green", "purple"), lwd = 2)


#Compare ROC Curves and AUC
auc_1 <- auc(roc_1)
auc_2 <- auc(roc_2)
auc_3 <- auc(roc_3)
auc_4 <- auc(roc_4)

# Display AUC values
cat("AUC for Model 1:", auc_1, "\n")
cat("AUC for Model 2:", auc_2, "\n")
cat("AUC for Model 3:", auc_3, "\n")
cat("AUC for Model 4:", auc_4, "\n")

