#Question1
#a,b,c,d,e,f
read.csv("C:/Users/seema/OneDrive/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
customer_churn[is.na(customer_churn)]=0
sample.split(customer_churn$gender, SplitRatio = 0.65)-> split_forest2
subset(customer_churn, split_forest2==T)-> train_forest2
subset(customer_churn, split_forest2==F)-> test_forest2
randomForest(gender~tenure+MonthlyCharges, data = train_forest2, mtry=2, ntree=35)-> model_forest2
importance(model_forest2)
varImpPlot(model_forest2)
predict(model_forest2, new_data=test_forest2, type="class")-> result_forest2
table(test_forest2$gender, result_forest2)-> confusion_matrix2
((2465+0)/(2465+0+0+4578))*100
#Accuracy: 34.99%

#Question2
#a,b,c,d,e
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
customer_churn[is.na(customer_churn)]=0
sample.split(customer_churn$gender, SplitRatio = 0.65)-> split_forest2
subset(customer_churn, split_forest2==T)-> train_forest2
subset(customer_churn, split_forest2==F)-> test_forest2
randomForest(gender~tenure+MonthlyCharges, data = train_forest2, mtry=2, ntree=350)-> model_forest2
importance(model_forest2)
varImpPlot(model_forest2)
predict(model_forest2, newdata = test_forest2, type = "class")-> result_forest2
table(test_forest2$gender, result_forest2)-> confusion_matrix
confusion_matrix
((564+630)/(564+630+657+614))*100
#Accuracy: 48.43%

