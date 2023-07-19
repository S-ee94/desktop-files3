#Question1
sqldf("select OnlineBackup from customer_churn")-> customer_online_backup
View(customer_online_backup)

#Question2
sqldf("select StreamingTV,StreamingMovies,Contract from customer_churn")-> customer_streaming_contract
View(customer_streaming_contract)

#Question3
sqldf("select * from customer_churn where PaymentMethod='Mailed check'")-> customer_mail
View(customer_mail)

#Question4
sqldf("select * from customer_churn where gender='Female' and tenure=1 and PaymentMethod='Mailed check'")-> customer_random_selection
View(customer_random_selection)
