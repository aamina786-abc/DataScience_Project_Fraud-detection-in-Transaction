install.packages(c("tidyverse"))


install.packages(c("caret"))


install.packages(c("ROSE"))


# For older R versions
install.packages("DMwR")

# Or for newer R versions
install.packages("DMwR2")


install.packages("themis")



install.packages(c("pROC"))


install.packages(c("randomForest"))


install.packages(c("ggplot2"))


getwd()


data <- read.csv("creditcard.csv")
str(data)
head(data)


table(data$Class)
prop.table(table(data$Class))


colSums(is.na(data))


data$Amount <- scale(data$Amount)
data$Time <- scale(data$Time)

library(ggplot2)

ggplot(data, aes(x = as.factor(Class))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Class", y = "Count", title = "Fraud vs Legit Transactions")


ggplot(data, aes(x = as.factor(Class), y = Amount)) +
  geom_boxplot() +
  labs(title = "Transaction Amount by Class")


set.seed(123)


library(caret)

trainIndex <- createDataPartition(data$Class, p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]




# Remove attributes from numeric columns
num_cols <- sapply(train_clean, is.numeric)
train_clean[num_cols] <- lapply(train_clean[num_cols], function(x) as.numeric(x))

# Apply ROSE again
library(ROSE)
train_balanced <- ROSE(Class ~ ., data = train_clean, seed = 1)$data

# Check the distribution
table(train_balanced$Class)







log_model <- glm(Class ~ ., data = train_balanced, family = binomial)
summary(log_model)



str(train)   # see types in training data
str(test)    # see types in test data

test$Time <- as.numeric(test$Time)
test$Amount <- as.numeric(test$Amount)



test$Class <- as.factor(test$Class)  # if needed


log_pred_prob <- predict(log_model, test, type = "response")

log_pred <- ifelse(log_pred_prob > 0.5, 1, 0)

install.packages("caret")
library(caret)


confusionMatrix(as.factor(log_pred), as.factor(test$Class))



install.packages("randomForest")


library(randomForest)

rf_model <- randomForest(
  Class ~ ., 
  data = train_balanced, 
  ntree = 100,
  importance = TRUE
)



rf_pred <- predict(rf_model, test)
confusionMatrix(rf_pred, as.factor(test$Class))



roc_obj <- roc(test$Class, as.numeric(log_pred_prob))
plot(roc_obj, col = "blue", main = "ROC Curve")
auc(roc_obj)




suspicious_txns <- test[log_pred == 1, ]
head(suspicious_txns)










