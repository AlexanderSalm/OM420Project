#source("DataCleaning.R")

# Transactions by Month
#ggplot(transactions) + geom_bar(mapping=aes(x=month)) + labs(x="Month", y="Transaction Count") + ggtitle("Individual transactions (not accounting for quantity)")\
transaction_quantities <- aggregate(transactions$quantity, by=list(Category=transactions$month), FUN=sum)
ggplot(transaction_quantities) + geom_bar(mapping=aes(x=Category, y=x), stat="identity") + labs(x="Month", y="Transaction Quantity") + ggtitle("Total Items Purchased (accounting for quantity)")

# Plots for transactions by month
#ggplot(returns) + geom_bar(mapping=aes(x=month)) + labs(x="Month", y="Return Count") + ggtitle("Individual return instances (not accounting for quantity)")
return_quantities <- aggregate(returns$quantity, by=list(Category=returns$month), FUN=sum)
ggplot(return_quantities) + geom_bar(mapping=aes(x=Category, y=x), stat="identity") + labs(x="Month", y="Return Quantity") + ggtitle("Total Items Returned (accounting for quantity)")

# Rate of Return Plot
rate_of_return <- data.frame(Month = c(), Rate = c())
for(i in 1:nrow(transaction_quantities)){
  rate_of_return <- bind_rows(rate_of_return, data.frame(
    Month = transaction_quantities$Category[i], 
    Rate  = (return_quantities$x[i] / transaction_quantities$x[i]) * 100
  ))
}
ggplot(rate_of_return) + geom_bar(mapping=aes(x=Month, y=Rate), stat="identity") + labs(x="Month", y="Rate of Return (%)") + ggtitle("Rate of Return (%) by Month") + coord_cartesian(ylim = c(0.8, 1.1))

# Return frequency by quantity
returns_quantity_factor <- returns
returns_quantity_factor$quantity <- factor(returns_quantity_factor$quantity)
ggplot(returns_quantity_factor) + geom_bar(mapping=aes(x=quantity)) + labs(x="Return Quantity", y="Return Count") + ggtitle("Return count by Quantity")

# Return rate by Product ID
return_pid_quantities <- aggregate(returns$quantity, by=list(Category=returns$product_id), FUN=sum)
products_sold <- aggregate(transactions$quantity, by=list(Category=transactions$product_id), FUN=sum)
rate_of_return_pid <- merge(x = return_pid_quantities, y = products_sold, by = "Category", all = TRUE)
rate_of_return_pid$rate <- (rate_of_return_pid$x.x / rate_of_return_pid$x.y) * 100
rate_of_return_pid <- arrange(rate_of_return_pid, rate)
ggplot(rate_of_return_pid) + geom_bar(mapping=aes(x=Category, y=rate), stat="identity") + labs(x="Product ID", y="Rate of Return (%)") + ggtitle("Rate of Return (%) by Product ID")







