#Question1
#a, b
ggplot(data = customer_churn, aes(x=PhoneService)) + geom_bar(fill="pink", col="peru")

#Question2
#a, b
ggplot(data = customer_churn, aes(x=InternetService,fill=InternetService)) + geom_bar(position = "identity")
ggplot(data = customer_churn, aes(x=InternetService,fill=Contract)) + geom_bar(position = "identity")

#Question3
#a
ggplot(data = customer_churn,aes(x=TechSupport, fill=Churn)) + geom_bar()
