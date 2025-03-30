# Run data prep and cleaning if not done yet
if(!exists("finished_cleaning_flag")){
  source("DataCleaning.R")
}
# Clear all plots
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)

# Transactions by Month
#ggplot(transactions) + geom_bar(mapping=aes(x=month)) + labs(x="Month", y="Transaction Count") + ggtitle("Individual transactions (not accounting for quantity)")\
ggplot(transaction_quantities) + geom_bar(mapping=aes(x=Category, y=x), stat="identity") + labs(x="Month", y="Transaction Quantity") + ggtitle("Total Items Purchased (accounting for quantity)")

# Plots for transactions by month
#ggplot(returns) + geom_bar(mapping=aes(x=month)) + labs(x="Month", y="Return Count") + ggtitle("Individual return instances (not accounting for quantity)")
ggplot(return_quantities) + geom_bar(mapping=aes(x=Category, y=x), stat="identity") + labs(x="Month", y="Return Quantity") + ggtitle("Total Items Returned (accounting for quantity)")

# Rate of Return Plot
ggplot(rate_of_return) + geom_bar(mapping=aes(x=Month, y=Rate), stat="identity") + labs(x="Month", y="Rate of Return (%)") + ggtitle("Rate of Return (%) by Month") + coord_cartesian(ylim = c(0.8, 1.1))

# Return frequency by quantity
ggplot(returns_quantity_factor) + geom_bar(mapping=aes(x=quantity)) + labs(x="Return Quantity", y="Return Count") + ggtitle("Return count by Quantity")

# Return rate by Product ID
ggplot(rate_of_return_pid) + geom_bar(mapping=aes(x=Category, y=rate), stat="identity") + labs(x="Product ID", y="Rate of Return (%)") + ggtitle("Rate of Return (%) by Product ID")

ggplot(rate_of_return_pid) + geom_bar(mapping=aes(x=reorder(Category, rate), y=rate), stat="identity") + labs(x="Product ID", y="Rate of Return (%)") + ggtitle("Arranged Rate of Return (%) by Product ID")

# Return rate by Store ID
ggplot(rate_of_return_store) + geom_bar(mapping=aes(x=Category, y=rate), stat="identity") + labs(x="Store", y="Return Rate (%)") + ggtitle("Rate of Return (%) by Store")










