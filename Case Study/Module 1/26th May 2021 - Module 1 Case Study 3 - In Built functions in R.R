
#Question1
#a
head(customer_churn$PhoneService, 4)

#b
head(customer_churn$Contract, 8)

#Question2
#a
tail(customer_churn$TotalCharges,1)

#b
tail(customer_churn$tenure, 5)

#Question3
#average 
mean(customer_churn$tenure)
#minimum
min(customer_churn$tenure)
#maximum
max(customer_churn$tenure)
#Range
range(customer_churn$tenure)

#Question4
sample(customer_churn$TotalCharges, 10)

#Question5
table(customer_churn$PaymentMethod, customer_churn$Contract)