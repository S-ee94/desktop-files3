#Diamond Dataset

library(ggplot2)
View(diamonds)

library(dplyr)

#Task 1 - Extract the data where price>1000 and cut=ideal
diamonds%>% filter(price>1000 & cut=="Ideal")-> diamonds_1000_Ideal
View(diamonds_1000_Ideal)

#Task 2 - Make a Scatter Plot between 'price'(x-axis) and 'carat'(y-axis) and color should be determined by cut
#using ggplot
ggplot(diamonds,aes(x=price,y=carat,col=cut)) + geom_point()

#Task 3 - Introduce 25% missing values in this 'iris' dataset and impute the 'sepal.length' column with mean and
#'petal.length' column with median
View(iris)
library(missForest)
iris.mis<- prodNA(iris,noNA = 0.25)
View(iris.mis)
library(Hmisc)
iris.mis$Sepal.Length<- with(iris.mis,impute(Sepal.Length,mean))
iris.mis$Petal.Length<- with(iris.mis,impute(Petal.Length,median))
View(iris.mis)

#Task 4 - What do you understand by linear regression?
#It is a supervised learning algorithm which helps us in finding the linear relationship between dependent and
#independent variables.
# On is the predictor or the independent variable and the other is the response or the dependent variable.
#example- a telecom company,we want to know the linear relationship between the monthly charges of a customer w.r.t
#tenure.
#y=Monthly Charges and x= tenure

#Task 5 - Implement Simple Linear Regression in R on the mtcars dataset where the dependent variable is 'mpg' and 
# independent variable is 'disp'
View(mtcars)
library(caret)
split_tag<- createDataPartition(mtcars$mpg, p=0.65, list = F)
mtcars[split_tag,]-> train
mtcars[-split_tag,]-> test
lm(mpg~disp, data = train)-> mod_mtcars
predict(mod_mtcars,newdata = test)-> pred_mtcars
head(pred_mtcars)

#Task 6 - Calculate the RMSE for the model built
cbind(Actual=test$mpg,Predicted=pred_mtcars)-> final_data
as.data.frame(final_data)-> final_data
final_data
final_data$Actual - final_data$Predicted-> error
cbind(final_data,error)-> final_data
final_data
sqrt(mean((final_data$error)^2))-> rmse
rmse

#Task 7 - Implement the Simple Linear Regression in Python 

#Task 8 - What do you understand by Logistic Regression?
# It is a classification algorithm which is used to predict a binary outcome.
#example- temperature+humidity = Rain, y=Rain, x=temp+humidity. predicted y lies in between 0 & 1

#Task 9 - Implement Logistic Regression on the 'heart' dataset in R where dependent variable is 'target' and 
# independent variable is 'age'
#Note: If Residual Deviance is less than the Null Deviance then the relationship between the y and x is strong 
# and the model is good

#Task 10 - What is a Confusion Matrix? 
# A Confusion matrix (CM) is a table which is used to estimate the performance of a model
# A CM can be used to evaluate the accuracy of the model built. It calculates a cross-tabulation of observed & 
# predicted classes.

#Task 11 - Build a CM for the model where the threshold value for the probability of predicted values is 0.6,
# & also find the accuracy of the model

#Task 12 - What do you understand by True Positive Rate and False Positive Rate?
# True Positive Rate = Sensitivity - measure of percentage of actual values that have been correctly identified.
# Formula = True Positives/Positives
# False Positive Rate = Probability of falsely rejecting the null hypothesis test, calculated as the ratio between 
#number of negative events wrongly categorized as Positive.
#Formula = False Positives/Negatives

#Task 13 - Explain what is ROC Curve? ROC - Receiver Operating Characteristic
# It is basically a plot between True Positive Rate and False Positive Rate and it helps us to find the right 
#trade-off between true positive rate and false positive rate and probability thresholds of the predicted values
#Note: Whichever curve has greater area under it, that would be the better model.

#Task 14 - Build ROC Curve for the model built
library(ROCR)

#Task 15 - Build a logistic regression model on customer_churn using Python, y=Churn and x=MonthlyCharges

#Task 16 - What do you understand by decision tree?
# Decision tree is a supervised learning algorithm which is used for both classification & Regression models
# It is a flow chart like structure, dependent variable can be both numerical & categorical

#Task 17 - Build a decision tree model on 'iris' dataset where dependent variables is 'Species' and all other
# columns are independent variables. Find the accuracy of the model built
View(iris)
library(party)
library(caret)
split_tag<- createDataPartition(iris$Species,p=0.65, list = F)
iris[split_tag,]-> train_set
iris[-split_tag,]-> test_set
mytree<- ctree(Species~., train_set)
plot(mytree)
predict(mytree,test_set,type="response")-> my_pred
table(test_set$Species,my_pred)-> confusion_matrix
(sum(diag(confusion_matrix))/sum(confusion_matrix))*100
#Accuracy = 94.11765%

#Task 18 - What do you know about Random Forest Model? Also, explain the working mechanism of the RF Model.
# It is an ensemble model, combines multiple models(decision trees) together to get the final output.
# Mechanism - 
#sample replacement from A to A1 n times, & so on till Ax
#In RF, each time a node is split into decision tree, not all columns are provided to RF algorithm, only random
# subset of these n columns will be made available to the algorithm

#Task 19 - Build a RF model on top of 'CTG' dataset where 'NSP' is dependent variable and all others columns are
# independent variables




















































