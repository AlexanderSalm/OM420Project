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

# Add purchase columns (takes a long time to run!)
customers_clean <- add_purchases_columns(customers_clean, transactions)