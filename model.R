# Load important libraries
# For data manipulation and visualization
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")  # For model building and evaluation
install.packages("ROSE")   # For handling imbalanced data
install.packages("randomForest")

library(ggplot2)

# Reading a CSV file
data <- read.csv("C:/Users/ertug/Desktop/CC Fraud Detection/data/creditcard.csv", header = TRUE, sep = ",")

# Display the first few rows of the data
head(data)

# Check structure and summary statistics
str(data)
summary(data)

# Check for missing values
sum(is.na(data))

# Class distribution (imbalanced labels)
table(data$Class)


# Visualize fraud (Class 1) vs. non-fraud (Class 0)
ggplot(data, aes(x = Class)) +
  geom_bar() +
  ggtitle("Class Distribution (Fraud vs. Non-Fraud)") +
  xlab("Class") +
  ylab("Count")


# Standardizing numerical features
data_scaled <- data
data_scaled[, -ncol(data_scaled)] <- scale(data[, -ncol(data)])  # Exclude the label column (Class)
