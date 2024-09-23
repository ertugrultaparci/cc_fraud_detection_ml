# Load necessary libraries
library(gbm)      # For Gradient Boosting Machine (GBM)
library(caret)    # For model training and evaluation
library(dplyr)    # For data manipulation
library(ggplot2)  # For data visualization

# Read the dataset
data <- read.csv("C:/Users/ertug/Desktop/cc_fraud_detection_ml/data/creditcard.csv", header = TRUE, sep = ",")

# Check the structure of the dataset
str(data)

# Check for missing values
sum(is.na(data))

# Convert the Class variable to numeric (0 and 1)
data$Class <- as.numeric(as.factor(data$Class)) - 1  # Convert factor to numeric (0/1)

# Ensure that the Class variable is numeric and contains only 0 and 1
if(!all(data$Class %in% c(0, 1))) {
  stop("Class variable must be numeric and contain only 0 and 1.")
}

# Scale the feature variables (excluding the Class variable)
data_scaled <- data %>%
  mutate(across(where(is.numeric) & !c(Class), scale))  # Scale only numeric columns except Class

# Split the dataset into training and test sets
set.seed(42)  # For reproducibility
trainIndex <- createDataPartition(data_scaled$Class, p = .8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data_scaled[trainIndex, ]
data_test <- data_scaled[-trainIndex, ]

# Define predictors and response
X_train <- data_train %>% select(-Class)  # Features for training
y_train <- data_train$Class  # Target variable for training
X_test <- data_test %>% select(-Class)  # Features for testing
y_test <- data_test$Class  # Target variable for testing

# Fit the Gradient Boosting Model using gbm
set.seed(42)  # For reproducibility
gbm_model <- gbm(
  formula = Class ~ .,  # Your response variable
  data = data_train,
  distribution = "bernoulli",  # For binary classification
  n.trees = 300,  # Number of boosting iterations
  interaction.depth = 3,  # Depth of each tree
  n.minobsinnode = 10,  # Minimum number of observations in the terminal nodes
  shrinkage = 0.01,  # Learning rate
  verbose = TRUE
)

# Predict on the test set
y_pred <- predict(gbm_model, newdata = X_test, n.trees = gbm_model$n.trees, type = "response")

# Convert predictions to binary
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)

# Evaluate the model
confusion_matrix <- table(y_test, y_pred_class)
print(confusion_matrix)

# Calculate performance metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Convert the confusion matrix to a data frame
confusion_df <- as.data.frame(confusion_matrix)
colnames(confusion_df) <- c("Actual", "Predicted", "Frequency")  # Rename columns for clarity

# Optionally, visualize the results
ggplot(confusion_df, aes(x = Actual, y = Frequency, fill = as.factor(Predicted))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Actual Class", y = "Frequency", fill = "Predicted Class") +
  theme_minimal()
