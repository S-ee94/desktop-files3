#Question1
#a
customer_churn$MonthlyCharges[5]+5->customer_churn$MonthlyCharges[5]
View(customer_churn)

#b
customer_churn$MonthlyCharges[6]-9.65->customer_churn$MonthlyCharges[6]
View(customer_churn)

#c
customer_churn$MonthlyCharges[1]*3->customer_churn$MonthlyCharges[1]
View(customer_churn)

#d
customer_churn$MonthlyCharges[37]/3->customer_churn$MonthlyCharges[37]
View(customer_churn)

#Question2
#a
customer_churn$tenure[1]>customer_churn$tenure[10]

#b
customer_churn$tenure[3]==customer_churn$tenure[5]

#Question3
#a
#approach 1: 
table(customer_churn$TechSupport,customer_churn$StreamingTV)

or 
#approach 2:
(customer_churn$TechSupport=='Yes' & customer_churn$StreamingTV=='Yes')-> value
as.data.frame(value)->value
View(value)
subset(customer_churn,value==TRUE)-> new_data
View(new_data)


#b
(customer_churn$InternetService=='DSL' | customer_churn$InternetService=='Fiber optic')-> k
as.data.frame(k)->k
View(k)
subset(customer_churn$InternetService,k==TRUE)-> Internet_dsl_fiber
View(Internet_dsl_fiber)
as.data.frame(Internet_dsl_fiber)->Internet_dsl_fiber
View(Internet_dsl_fiber)

