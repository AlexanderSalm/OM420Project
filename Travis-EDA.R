rm(list=ls())

library(readxl)
library(dplyr)
library(tidyverse)

{customers <- read.csv("MavenMarket_Customers.csv")
  # metadata <- read.xls("MavenMarket_Metadata.xls")
  products <- read.csv("MavenMarket_Products.csv")
  regions <- read.csv("MavenMarket_Regions.csv")
  returns <- read.csv("MavenMarket_Returns_1997-1998.csv")
  stores <- read.csv("MavenMarket_Stores.csv")
  transactions97 <- read.csv("MavenMarket_Transactions_1997.csv")
  transactions98 <- read.csv("MavenMarket_Transactions_1998.csv")
}

library(lubridate)
# Customers
str(customers)


customers_clean <- customers %>% 
  mutate(birthdate = mdy(birthdate), # Date
         customer_country = as.factor(customer_country), #Factor
         marital_status = as.factor(marital_status), # Factor
         yearly_income = as.factor(yearly_income), # Factor b/c range
         gender = as.factor(gender), # factor
         education = as.factor(education), # factor
         acct_open_date = mdy(acct_open_date), # Date
         member_card = as.factor(member_card), # Factor
         occupation = as.factor(occupation),# factor
         homeowner = as.factor(homeowner)) # factor

summary(customers_clean)


# Products
str(products)

products_clean <- products %>% 
  mutate(recyclable = if_else(is.na(recyclable), 0, 1),
         low_fat = if_else(is.na(low_fat), 0, 1))

# Regions is already clean

regions_clean <- regions

# Returns

returns_clean <- returns %>% 
  mutate(return_date = mdy(return_date),
         store_id = as.factor(store_id))

# Stores

str(stores)

stores_clean <- stores %>% 
  mutate(store_id = as.factor(store_id),
         store_type = as.factor(store_type),
         store_name = as.factor(store_name),
         store_state = as.factor(store_state),
         store_country = as.factor(store_country),
         first_opened_date = mdy(first_opened_date),
         last_remodel_date = mdy(last_remodel_date)
  )

# Transactions97
str(transactions97)

transactions97_clean <- transactions97 %>% 
  mutate(transaction_date = mdy(transaction_date),
         store_id = as.factor(store_id),
         stock_date = mdy(stock_date)
  )



# Transactions98
str(transactions98)

transactions98_clean <- transactions98 %>% 
  mutate(transaction_date = mdy(transaction_date),
         store_id = as.factor(store_id),
         stock_date = mdy(stock_date)
  )

transactions_clean <- bind_rows(transactions97_clean, transactions98_clean)

rm("customers", "returns","stores", "products", "regions", "transactions97", "transactions98")

library(ggplot2)
ggplot(returns_clean, aes(x = product_id)) +
  geom_bar() +
  labs(title = "Product Returns by Product Type", x = "Product ID", y = "Count of Returns") +
  theme_minimal()

products_returns <- left_join(products_clean, returns_clean, by = "product_id") %>% 
  mutate(quantity = as.factor(if_else(is.na((quantity)), 0, quantity))) %>% 
  rename(`quantity_returned` = "quantity")

products_returns

ggplot(products_returns, aes(x = product_retail_price, fill = quantity_returned)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  labs(title = "Price Distribution for Returned vs. Non-Returned Products", x = "Retail Price", y = "Count") +
  theme_minimal()

library(lubridate)

returns_clean$month <- floor_date(returns_clean$return_date, "month")

ggplot(returns_clean, aes(x = month)) +
  geom_line(stat = "count", color = "blue") +
  labs(title = "Monthly Trend of Product Returns", x = "Month", y = "Return Count") +
  theme_minimal()