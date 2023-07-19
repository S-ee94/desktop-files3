#Question1
#a
ggplot(data = customer_churn, aes(y=TotalCharges,x=tenure)) + geom_point(col='wheat3')

#b
ggplot(data = customer_churn, aes(y=TotalCharges,x=tenure,col=PaymentMethod)) + geom_point()

#c
ggplot(data = customer_churn, aes(y=TotalCharges,x=tenure,col=gender)) + geom_point()

#d
ggplot(data = customer_churn, aes(y=TotalCharges,x=tenure,col=Dependents)) + geom_point(shape=10)

#Question2
#a
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges)) + geom_point(col="yellowgreen")

#b
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges, col=InternetService)) + geom_point()

#c
ggplot(data = customer_churn,aes(y=tenure,x=MonthlyCharges, col=Contract)) + geom_point()











