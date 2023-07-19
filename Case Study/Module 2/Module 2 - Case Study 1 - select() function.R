#Question1
#a
library(dplyr)
select(customer_churn, 5)-> customer_5
View(customer_5)

#or solution2
customer_churn%>%select(5)->customer_5

#b
select(customer_churn, 15)-> customer_15
View(customer_15)

#or solution2
customer_churn%>%select(15)->customer_15

#Question2
select(customer_churn, 3,6,9,12,15,18)->customer_3_multiple
View(customer_3_multiple)

#Question3
select(customer_churn, 10:20)-> c_10_20
View(c_10_20)

#Question4
customer_churn%>%select(starts_with("P"))->customer_P
View(customer_P)

#Question5
customer_churn%>%select(ends_with("S"))-> customer_S
View(customer_S)








