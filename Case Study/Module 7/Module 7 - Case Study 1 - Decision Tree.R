#Question1
#a,b,c,d,e,f
read.csv("C:/Users/seema/OneDrive/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE) -> customer_churn
View(customer_churn)
#set.seed helps to have fixed set of values while splitting
set.seed(1)
sample.split(customer_churn$Dependents, SplitRatio = 0.70)-> split_tag1
subset(customer_churn, split_tag1==T)-> train1
subset(customer_churn, split_tag1==F)-> test1

#using rpart package
library(rpart)
library(rpart.plot)
#single node tree cannot be plotted if tree package is used, hence using rpart package

#creating the decision tree model using rpart
rpart(Dependents~Partner, data = train1, method = "class")-> mod_tree1
rpart.plot(mod_tree1,type = 5, tweak = 0.75)
predict(mod_tree1, newdata = test1, type = "class")-> result_tree1

#or
  
#creating the decision tree model using tree package 
tree(Dependents~Partner, data = train1)-> mod_tree1
plot(mod_tree1)
text(mod_tree1)
predict(mod_tree1, newdata = test1, type = "class")-> result_tree1
table(test1$Dependents, result_tree1)-> confusion_matrix1
confusion_matrix1
((956+534)/(956+534+524+99))*100
#Accuracy = 70.51%

#or using diag function
(sum(diag(confusion_matrix1))/sum(confusion_matrix1))*100
#Accuracy = 70.51%



#Question2
#a,b,c,d,e,f
sample.split(customer_churn$Dependents, SplitRatio = 0.70)-> split_tag2
subset(customer_churn, split_tag1==T)-> train2
subset(customer_churn, split_tag1==F)-> test2
tree(Dependents~Partner+InternetService, data = train2)-> mod_tree2
plot(mod_tree2)
text(mod_tree2)
predict(mod_tree2, newdata = test2, type = "class")-> result_tree2
table(test2$Dependents, result_tree2)-> confusion_matrix2
(1257+356)/(1257+356+223+277)
#Accuracy = 76.33%

#or
(sum(diag(confusion_matrix2))/sum(confusion_matrix2))*100
#Accuracy = 76.33%


