#Question1
if(customer_churn$PaymentMethod[6] == 'Electronic check'){
  print("Yes, the Payment method is electronic check")
}

#Question2
if(customer_churn$Contract[12] == 'Month-to-month'){
  print("The Contract is on a Month-to-month basis")
}else if(customer_churn$Contract[12] == 'One year'){
  print("The contract is on a yearly basis")
}else if(customer_churn$Contract[12] == 'Two year'){
  print("The contract is on two year basis")
}else{
  print("Output Invalid")
}

#Question3
switch (as.character(customer_churn$gender[6]),
  "Male" = customer_churn$MonthlyCharges[6]*0.8->customer_churn$MonthlyCharges[6],
  "Female" = customer_churn$MonthlyCharges[6]*0.5->customer_churn$MonthlyCharges[6]
)
View(customer_churn)

#Question4
count=0
for (i in 1:nrow(customer_churn)) {
  if(customer_churn$InternetService[i] == 'DSL'){
    count=count+1
  }
}
count

#Question5
count=0
i=1
while (i<=nrow(customer_churn)) {
  if(customer_churn$tenure[i]== 2){
    count=count+1
  }
  i=i+1
}
count


