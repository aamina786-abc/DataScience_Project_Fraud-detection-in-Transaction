STEP 0: Install & Load Required Libraries
install.packages(c("tidyverse"))
install.packages(c("caret"))
install.packages(c("ROSE"))
# For older R versions
#install.packages("DMwR")
# Or for newer R versions
#install.packages("DMwR2")
install.packages("themis")
install.packages(c("pROC"))
install.packages(c("randomForest"))
install.packages(c("ggplot2"))

#1 Load the dataset
getwd()
data <- read.csv("creditcard.csv")
str(data)
head(data)

#2 Understand the Data
table(data$Class)
prop.table(table(data$Class))

#3 Data Cleaning
colSums(is.na(data))

#3.a Scale Amount & Time
data$Amount <- scale(data$Amount)
data$Time <- scale(data$Time)

#4 Exploratory Data Analysis (EDA)
library(ggplot2)
ggplot(data, aes(x = as.factor(Class))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Class", y = "Count", title = "Fraud vs Legit Transactions")

#4.a Transaction Amount vs Fraud
library(ggplot2)
library(ggplot2)
set.seed(123)
data <- data.frame(
  Class = c(rep(0, 100), rep(1, 20)),               # 0 = Legit, 1 = Fraud
  Amount = c(rnorm(100, mean=50, sd=10), rnorm(20, mean=150, sd=30))
)
# The Fixed Plotting Code
ggplot(data, aes(x = factor(Class), y = Amount, fill = factor(Class))) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  labs(
    title = "Transaction Amount by Class",
    x = "Transaction Class (0 = Legit, 1 = Fraud)",
    y = "Transaction Amount (Scaled)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#5 rain-Test Split
set.seed(123)
library(caret)
trainIndex <- createDataPartition(data$Class, p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

#6 Handle Class Imbalance
# Remove attributes from numeric columns
num_cols <- sapply(train_clean, is.numeric)
train_clean[num_cols] <- lapply(train_clean[num_cols], function(x) as.numeric(x))
# Apply ROSE again
library(ROSE)
train_balanced <- ROSE(Class ~ ., data = train_clean, seed = 1)$data
# Check the distribution
table(train_balanced$Class)

#8 Random Forest Model
library(randomForest)

# Train classification Random Forest
rf_model <- randomForest(
  Class ~ .,
  data = train_balanced,
  ntree = 100,
  importance = TRUE
)

# Verify model type
rf_model$type   # MUST be "classification"

library(caret)
# Class prediction
rf_pred <- predict(rf_model, newdata = test)
confusionMatrix(rf_pred, test$Class)

# Probability prediction (for ROC)
rf_prob <- predict(rf_model, newdata = test, type = "prob")[, 2]

library(pROC)
# ROC Curve
roc_rf <- roc(test$Class, rf_prob)
plot(roc_rf, col = "red", main = "Random Forest ROC Curve")

# AUC value
auc(roc_rf)







