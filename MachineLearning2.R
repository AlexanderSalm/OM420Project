rm(list=ls())
# Clear all plots
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)

library(dplyr)
library(lubridate)
# Load
tx_97 <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Transactions_1997.csv")
tx_98 <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Transactions_1998.csv")
returns <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Returns_1997-1998.csv")
products <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Products.csv")
stores <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Stores.csv")
regions <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Regions.csv")
customers <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Customers.csv")

# Combine tx
transactions <- bind_rows(tx_97, tx_98) %>%
  mutate(transaction_date = mdy(transaction_date),
         key = paste(product_id, store_id))

# returns key
returns <- returns %>%
  mutate(return_date = mdy(return_date),
         key = paste(product_id, store_id))

# left join key
returns_short <- returns %>% select(key, return_date)
transactions <- left_join(transactions, returns_short, by = "key")

# label returns
transactions <- transactions %>%
  mutate(
    Returned = ifelse(!is.na(return_date), 1, 0)
  ) %>%
  select(-return_date, -key)

# Make final dataset
transactions <- transactions %>%
  left_join(products, by = "product_id") %>%
  left_join(stores, by = "store_id") %>%
  left_join(regions, by = "region_id") %>%
  left_join(customers, by = "customer_id")

# Feature engineering. We want to create a variable for season, profit margin, and store age.
transactions <- transactions %>%
  mutate(
    season = quarter(transaction_date),
    profit_margin = product_retail_price - product_cost,
    store_age = as.numeric(difftime(transaction_date, mdy(first_opened_date), units = "days"))
  )

# Final model dataset
model_data <- transactions %>%
  select(
    Returned, quantity, season, product_brand, product_retail_price, product_cost, profit_margin, product_weight, store_type, total_sqft, grocery_sqft, sales_district, sales_region, store_age, gender, marital_status, yearly_income, total_children, num_children_at_home, education, homeowner
  ) %>%
  na.omit()

library(randomForest)

# Convert character columns to factors
model_data <- mutate_if(model_data, is.character, as.factor)
model_data <- model_data %>% 
  mutate(
    Returned = as.factor(Returned),
    season = as.factor(season),
    total_children = as.factor(total_children),
    num_children_at_home = as.factor(num_children_at_home)
  )

# frequency of each brand
brand_counts <- table(model_data$product_brand)
# RF only handles up to 50 categories, let's select top 50
top_brands <- names(sort(brand_counts, decreasing = TRUE))[1:50]

# keep top brands, label the rest as "Other"
model_data <- model_data %>%
  mutate(product_brand = ifelse(product_brand %in% top_brands, as.character(product_brand), "Other")) %>%
  mutate(product_brand = as.factor(product_brand))

# First split: 70% training, 30% remaining
set.seed(123)
sample_index <- sample(1:nrow(model_data), size = 0.7 * nrow(model_data))
train_data <- model_data[sample_index, ]
remaining_data <- model_data[-sample_index, ]

# Second split: Divide the remaining 30% equally into validation and test sets (15% each)
set.seed(123)
val_index <- sample(1:nrow(remaining_data), size = 0.5 * nrow(remaining_data))
validation_data <- remaining_data[val_index, ]
test_data <- remaining_data[-val_index, ]

#bag_model <- randomForest(Returned ~ ., data=train_data, mtry = ncol(train_data) - 1, importance=T, na.action=na.omit)

data_train <- train_data
data_test <- test_data
data_val <- validation_data

# Create Tree====
fit <- rpart(Returned~., data=data_train, method="class", parms = list(split = "information"), minsplit = 2, minbucket = 1, cp=-1)

#rpart.plot(fit)
printcp(fit)

# Find results====
pred <- predict(fit, data_test, type="class")
confusion_matrix <- table(data_test$Returned, pred)
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
  results <- table(data_val$Returned, prediction)
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

# Print results
confusion_matrix <- table(data_test$Returned, opt_pred)
print(confusion_matrix)
tn <- confusion_matrix[1]
fn <- confusion_matrix[2]
fp <- confusion_matrix[3]
tp <- confusion_matrix[4]

print(paste("Error rate:  ", (fp + fn) / (tp + tn + fp + fn)))
print(paste("Accuracy:    ", (tp + tn) / (tp + tn + fp + fn)))
print(paste("Sensitivity: ", (tp) / (tp + fn)))
print(paste("Specificity: ", (tn) / (fp + tn)))
