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

# Import libraries
library(dplyr)
library(ggplot2)
library(tidyr)

customers      <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Customers.csv")
products       <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Products.csv")
regions        <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Regions.csv")
returns        <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Returns_1997-1998.csv")
stores         <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Stores.csv")
transactions97 <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Transactions_1997.csv")
transactions98 <- read.csv("MavenMarket CSV Files/CSV Files/MavenMarket_Transactions_1998.csv")

# Clean up customer yearly income, split into seperate columns
customers_yrincome <- separate(customers, col = yearly_income, into = c("yearly_income_min", "yearly_income_max"), sep = " - ")

# Remove entries with NA in yearly income (about 200 entries out of 10000)
customers_yrincome <- customers_yrincome[!is.na(customers_yrincome$yearly_income_max),]
customers_yrincome <- customers_yrincome[!is.na(customers_yrincome$yearly_income_min),]

# Remove $, K (if exists, * by 1000) and convert to numeric
customers_yrincome$yearly_income_max <- process_yearly_income(customers_yrincome$yearly_income_max)
customers_yrincome$yearly_income_min <- process_yearly_income(customers_yrincome$yearly_income_min)

#ggplot(customers) + geom_smooth(mapping=aes(x=))