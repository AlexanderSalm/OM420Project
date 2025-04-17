# Run data prep and cleaning if not done yet
if(!exists("finished_cleaning_flag")){
  source("DataCleaning.R")
}

# Clear all plots
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)

library(rpart)
library(rpart.plot)

# Prepare Data====
data <- transactions
data$customer_id <- NULL
data$stock_date <- NULL
data$is_return <- F

returns_marked <- returns
returns_marked$is_return <- T
names(returns_marked)[names(returns_marked) == 'return_date'] <- 'transaction_date'
data <- rbind(data, returns_marked)
data$is_return <- factor(data$is_return)

# Sample====
percen_train <- 2/3
set.seed(123)
id <- sample(1:nrow(data), percen_train * nrow(data))
data_train <- data[id, ]
data_tmp <- data[-id, ]
test_id <- sample(1:nrow(data_tmp), (1/2) * nrow(data_tmp))
data_test <- data_tmp[test_id,]
data_val <- data_tmp[-test_id,]

# Create Tree====
fit <- rpart(is_return~., data=data_train, method="class", parms = list(split = "information"), minsplit = 2, minbucket = 1, cp=-1)

#rpart.plot(fit)
printcp(fit)

# Find results====
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

# Analysis====
n_tree <- nrow(fit$cptable) 

# Initialize a result dataframe to store the number of nodes, 
# CP values, and validation error rates
df_res <- data.frame(nbr_node = 0, cp = fit$cptable[,1], error_rates_val = 0)

# Loop through each CP value to prune the tree and evaluate performance
for (ii in 1:n_tree){
  # Step 1: Prune the full tree using the ith CP value and save it as pruned.tree
  pruned.tree <- prune(fit, cp = fit$cptable[ii])
  
  # Step 2: Predict the 'high_nps' classification for the validation set (df_val)
  prediction <- predict(pruned.tree, data_val, type="class")
  
  # Step 3: Generate a classification confusion matrix to compare predictions with actual values 
  results <- table(data_val$is_return, prediction)
  print(results)
  
  # Step 4: Calculate the validation error rate from the confusion matrix and save it as error_rate_val
  error_rate_val <- (results[1,2]+results[2,1])/nrow(data_val)
  print(error_rate_val)
  
  # Store the computed validation error rate
  df_res[ii, 'error_rates_val'] <- error_rate_val 
  
  # Store the number of nodes in the pruned tree 
  df_res[ii, 'nbr_node'] <- nrow(pruned.tree$frame)
}

# Print the computed validation error rates
df_res[, 'error_rates_val']

ggplot(df_res) + 
  scale_x_log10() +
  geom_line(mapping=aes(x=nbr_node, y=error_rates_val)) 

# Prune by using the 2nd method
min_xerror <- min(fit$cptable[, "xerror"])
corres_xstd <- fit$cptable[which.min(fit$cptable[, "xerror"]), "xstd"]
benchmark <- min_xerror + corres_xstd
RowNum <- min(which(fit$cptable[, "xerror"] < benchmark))
opt <- fit$cptable[RowNum, "CP"]
opt_fit <- prune(fit, cp = opt)
opt_pred <- predict(opt_fit, data_test, type = "class")

rpart.plot(opt_fit)

# Print results
confusion_matrix <- table(data_test$is_return, opt_pred)
print(confusion_matrix)
tn <- confusion_matrix[1]
fn <- confusion_matrix[2]
fp <- confusion_matrix[3]
tp <- confusion_matrix[4]

print(paste("Error rate:  ", (fp + fn) / (tp + tn + fp + fn)))
print(paste("Accuracy:    ", (tp + tn) / (tp + tn + fp + fn)))
print(paste("Sensitivity: ", (tp) / (tp + fn)))
print(paste("Specificity: ", (tn) / (fp + tn)))
