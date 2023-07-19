#Question1
sample_n(customer_churn, 333)->customer_333
View(customer_333)

#Question2
sample_n(customer_churn, 1000)-> customer_1000
View(customer_1000)

#Question3
sample_frac(customer_churn, 0.23)-> customer_23_percent
View(customer_23_percent)

#Question4
count(customer_churn, PaymentMethod)

#Question5
count(customer_churn, Churn)

