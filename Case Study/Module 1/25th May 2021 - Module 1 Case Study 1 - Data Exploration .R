#Question 1
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science/R Programming/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn

#a 
View(customer_churn)
customer_churn
customer_churn$InternetService->customer_internet_service
View(customer_internet_service)
as.data.frame(customer_internet_service)->customer_internet_service

#b
View(customer_churn)
customer_churn$PaperlessBilling-> customer_Paperless_Billing
View(customer_Paperless_Billing)
as.data.frame(customer_Paperless_Billing)-> customer_Paperless_Billing

#c
View(customer_churn)
customer_churn$StreamingTV->customer_StreamingTV
View(customer_StreamingTV)
as.data.frame(customer_StreamingTV)-> customer_StreamingTV

#Question 2
customer_churn[,c(3,6,9)]->customer_random_columns
View(customer_random_columns)

#Question 3
customer_churn[,10:20]->customer_10_20
View(customer_10_20)

#Question 4
customer_churn[c(65,765,3726,7000),]->customer_random_rows
View(customer_random_rows)

#Question 5
customer_churn[655:6550,]->customer_continuous_rows
View(customer_continuous_rows)

#Question 6
customer_churn[c(10,100,1000),c(5,10,15)]->customer_random_data
View(customer_random_data)







