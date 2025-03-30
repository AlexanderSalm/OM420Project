# Run data prep and cleaning if not done yet
if(!exists("finished_cleaning_flag")){
  source("DataCleaning.R")
}

# Clear all plots
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)

library(rpart)
library(rpart.plot)

# Prepare Data
data <- transactions
data$customer_id <- NULL
data$stock_date <- NULL
data$is_return <- F

returns_marked <- returns
returns_marked$is_return <- T
names(returns_marked)[names(returns_marked) == 'return_date'] <- 'transaction_date'
data <- rbind(data, returns_marked)
data$is_return <- factor(data$is_return)

percen_train <- 8/10
set.seed(123)
id <- sample(1:nrow(data), percen_train * nrow(data))
data_train <- data[id, ]
data_test <- data[-id, ]

fit <- rpart(is_return~., data=data_train, method="class", parms = list(split = "information"), minsplit = 2, minbucket = 1)

rpart.plot(fit)
printcp(fit)

# Find results
pred <- predict(fit, data_test, type="class")
confusion_matrix <- table(data_test$is_return, pred)
print(confusion_matrix)
tn <- confusion_matrix[1]
fn <- confusion_matrix[2]
fp <- confusion_matrix[3]
tp <- confusion_matrix[4]

print(paste("Error rate:  ", (fp + fn) / (tp + tn + fp + fn)))
print(paste("Accuracy:    ", (tp + tn) / (tp + tn + fp + fn)))
print(paste("Sensitivity: ", (tp) / (tp + fn)))
print(paste("Specificity: ", (tn) / (fp + tn)))

print(fit$cptable)

