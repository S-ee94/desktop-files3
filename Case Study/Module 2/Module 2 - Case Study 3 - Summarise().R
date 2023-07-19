#Question1
summarise(customer_churn, "Median of Tenure"=median(tenure), "Variance of Tenure"=var(tenure), "SD of Tenure"=sd(tenure))

#Question2
summarise(customer_churn,median(MonthlyCharges), var(MonthlyCharges), sd(MonthlyCharges))

#Question3
customer_churn%>%group_by(PaymentMethod)%>%summarise("SD of Tenure"=sd(tenure))

#or
group_by(customer_churn, PaymentMethod)-> l
View(l)
summarise(l, sd(tenure))


#Question4
customer_churn%>%group_by(Contract)%>%summarise("Median of Monthly Charges"=median(MonthlyCharges))

#or
group_by(customer_churn, Contract)-> y
View(y)
summarise(y, median(MonthlyCharges))

#Question5
customer_churn%>%group_by(InternetService)%>%summarise("Variance of TC"=var(TotalCharges, na.rm = TRUE))

#or
group_by(customer_churn, InternetService)-> a
View(a)
summarise(a,var(TotalCharges, na.rm = TRUE))





