# Clear the working environment
# This will delete *EVERYTHING* in the current working environment.
# I want this script to be able to be run in isolation, so I do this to ensure
# I'm not secretly relying on something previously loaded
# The rm() function accepts a list and deletes everything in it from the current environment
# The ls() function returns everything in the current environment as a list
rm(list=ls())

# Function definitions
process_yearly_income <- function (arr){
  ret <- c()
  for(i in 1:length(arr)){
    val <- arr[i]
    yearly_has_k <- endsWith(val, "k") | endsWith(val, "K")
    val <- gsub("[^0-9.-]", "", val)
    val <- as.numeric(val)
    if(yearly_has_k){
      val <- val * 1000
    }
    ret <- append(ret, val)
  }
  return(ret)
}

add_purchases_columns <- function(customers, purchases){
  purchase_count <- c()
  for(i in 1:length(customers$customer_id)){
    id <- customers$customer_id[i]
    
    purchase_count <- append(purchase_count, sum(filter(purchases, customer_id==id)$quantity))
  }
  
  customers$purchase_count <- purchase_count
  return(customers)
}

# Import libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(slider)

# Load data
customers      <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Customers.csv")
products       <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Products.csv")
regions        <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Regions.csv")
returns        <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Returns_1997-1998.csv")
stores         <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Stores.csv")
transactions97 <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Transactions_1997.csv")
transactions98 <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Transactions_1998.csv")

# Clean up customer yearly income, split into seperate columns
customers_clean <- separate(customers, col = yearly_income, into = c("yearly_income_min", "yearly_income_max"), sep = " - ")

# Remove entries with NA in yearly income (about 200 entries out of 10000)
customers_clean <- customers_clean[!is.na(customers_clean$yearly_income_max),]
customers_clean <- customers_clean[!is.na(customers_clean$yearly_income_min),]

# Remove $, K (if exists, * by 1000) and convert to numeric
customers_clean$yearly_income_max <- process_yearly_income(customers_clean$yearly_income_max)
customers_clean$yearly_income_min <- process_yearly_income(customers_clean$yearly_income_min)

# Convert columns to correct datatypes
customers_clean$member_card <- factor(customers_clean$member_card)
customers_clean$education <- factor(customers_clean$education)
customers_clean$customer_country <- factor(customers_clean$customer_country)
customers_clean$occupation <- factor(customers_clean$occupation)

# Add an age column (as of Jan 01, 2025)
customers_clean$birthdate <- parse_date_time(customers_clean$birthdate, "m/d/y", tz="") #dmy - day, month, year HM - hour, minute
customers_clean$age <- as.double(difftime(parse_date_time("01/01/2025", "m/d/y", tz=""), customers_clean$birthdate, unit=c("days")))

# Create unified transactions
transactions <- rbind(transactions97, transactions98)

# Add month column to transactions
transactions$transaction_date <- parse_date_time(transactions$transaction_date, "m/d/y", tz="")
transactions$month <- factor(month(transactions$transaction_date))

# Add month column to returns
returns$return_date <- parse_date_time(returns$return_date, "m/d/y", tz="")
returns$month <- factor(month(returns$return_date))

# Plots for transactions by month
ggplot(transactions) + geom_bar(mapping=aes(x=month)) + labs(x="Month", y="Transaction Count") + ggtitle("Individual transactions (not accounting for quantity)")

# Plots for transaction quantities by month
transaction_quantities <- aggregate(transactions$quantity, by=list(Category=transactions$month), FUN=sum)
ggplot(transaction_quantities) + geom_bar(mapping=aes(x=Category, y=x), stat="identity") + labs(x="Month", y="Transaction Quantity") + ggtitle("Total Items Purchased (accounting for quantity)")

# Plots for transactions by month
ggplot(returns) + geom_bar(mapping=aes(x=month)) + labs(x="Month", y="Return Count") + ggtitle("Individual return instances (not accounting for quantity)")

# Plots for transaction quantities by month
return_quantities <- aggregate(returns$quantity, by=list(Category=returns$month), FUN=sum)
ggplot(return_quantities) + geom_bar(mapping=aes(x=Category, y=x), stat="identity") + labs(x="Month", y="Return Quantity") + ggtitle("Total Items Returned (accounting for quantity)")

# Summary Statistics
rolling_avg <- slide(return_quantities$x, mean, .before = 1, .after = 1)
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
for(i in 1:length(rolling_avg)){
  print(paste(month_names[i], "rolling average:", rolling_avg[i]))
}


# Add purchase columns (takes a long time to run!)
#customers_clean <- add_purchases_columns(customers_clean, transactions)

# Some yearly income plots
#ggplot(customers_clean) + geom_histogram(mapping=aes(x=yearly_income_max))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=member_card))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=education))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=customer_country))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=occupation))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=gender))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=total_children, group=total_children))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=yearly_income_max, y=num_children_at_home, group=num_children_at_home))
#ggplot(customers_clean) + geom_count(mapping=aes(x=yearly_income_max, y=purchase_count))

#ggplot(customers_clean) + geom_point(mapping=aes(x=yearly_income_min, y=yearly_income_max))

# Some age plots
#ggplot(customers_clean) + geom_histogram(mapping=aes(x=age))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=age, y=member_card))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=age, y=education))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=age, y=customer_country))
#ggplot(customers_clean) + geom_boxplot(mapping=aes(x=age, y=occupation))
#ggplot(customers_clean) + geom_smooth(mapping=aes(x=age, y=yearly_income_max))
#ggplot(customers_clean) + geom_smooth(mapping=aes(x=age, y=yearly_income_min))





