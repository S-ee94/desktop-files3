#Question1
#a,b,c
ggplot(data = customer_churn, aes(x=PaymentMethod)) + geom_bar(fill="burlywood4") + ggtitle("
Bar-Plot for Payment Method") + xlab("Payment Method")

#Question2
#a, b, c
ggplot(data = customer_churn, aes(x=TotalCharges)) + geom_histogram(fill="forestgreen") + ggtitle("
Histogram of Total Charges") + xlab("Total Charges")

#Question3
#a, b, c
ggplot(data = customer_churn, aes(x=TotalCharges)) + geom_density(fill="maroon") +  
ggtitle("Density plot for Total Charges") + xlab("Total Charges")

