#Question1
#a
ggplot(data = customer_churn, aes(x=tenure)) + geom_histogram(fill="mediumspringgreen")

#b
ggplot(data = customer_churn, aes(x=tenure)) + geom_histogram(fill="mediumspringgreen", col="azure")

#c
ggplot(data = customer_churn, aes(x=tenure)) + geom_histogram(fill="mediumspringgreen", col="azure", bins = 100)

#Question2
#a
ggplot(data = customer_churn,aes(x=MonthlyCharges,fill=PaymentMethod)) + geom_histogram(bins = 100)

#b
ggplot(data = customer_churn,aes(x=MonthlyCharges,fill=OnlineBackup)) + geom_histogram(bins = 100)


#Question3
#a
ggplot(data = customer_churn,aes(x=TotalCharges,fill=gender)) + geom_histogram(bins = 50)

#b
ggplot(data = customer_churn,aes(x=TotalCharges,fill=InternetService)) + geom_histogram(bins = 50)







