#EDA on Logistic Regression - Simple Logistic
#Customer Churn DataSet

#Step1: Import data. Check if your Y is cat/cont. If it is cont, linear reg, cat => logistic
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
dim(customer_churn)
str(customer_churn)
summary(customer_churn)
#y variable = Churn

#Step2: check for missing values
sum(is.na(customer_churn$SeniorCitizen))
sum(is.na(customer_churn$tenure))
sum(is.na(customer_churn$MonthlyCharges))
sum(is.na(customer_churn$TotalCharges))
#Note: Cancelling the Total Charges column as it has missing values


#Step3: Check for Outliers and remove them
ggplot(data = customer_churn, aes(x=MonthlyCharges)) + stat_boxplot(geom = "errorbar") + geom_boxplot()
#if there are outliers then execute the below
mean_ten<- mean(customer_churn$MonthlyCharges)
sd_ten<- sd(customer_churn$MonthlyCharges)
ul_ten<- mean_ten+2*sd_ten

#Step4: Check SD of every X1, X2, X3, X4 . if SD ~ 0 , then drop the feature
#Not required since the model is simple logistic

#Step5: Var significance --> Chisq test if p>0.05 , then drop the feature

#Step6: Var significance --> IV ( WOE )  if IV < 0.1 , then drop the feature

#Step7: Shortlist features based on steps 2,3,4,5,6
#2,3,4,5,6 executed

#Step8: Check multicollinearity using VIF
#Not required since the model is simple logistic

#Step9: Drop features with VIF > 5 one by one to arrive at the final list of features
#Not required since the model is simple logistic

#Step10: Split the data into train and test
sample.split(customer_churn$Churn, SplitRatio = 0.65)-> split_tag1
subset(customer_churn, split_tag1==T)-> train1
subset(customer_churn, split_tag1==F)-> test1

#Step11: Model fitting/training
lm(Churn~MonthlyCharges, data= train1, family="binomial")-> model1
predict(model1, newdata = test1, type = "response")-> result1

#Step12: Check for p -values and further drop insignificant features with p > .05
summary(model1)
# p values is less than 0.05 hence no need to drop the insignificant feature


#Step13: Fit the model again
# no need to fit the model in this case as we did not drop any feature

#Step14: Model Validation - Acc, sens , spec
#need to build a confusion matrix and get the accuracy
prediction(result1, test1$Churn)-> pred1
performance(pred1, "acc") -> acc1
plot(acc1)
table(test1$Churn, result1>0.72)
#Finding Accuracy
((1811+0)/(1811+0+0+654))*100
#Accuracy=73.46%

#Step15: Need to check the cutoff - ROC table and ROC Curve. Make sure your AUC is > 0.7
performance(pred1, "auc")-> auc1
View(auc1)
#Note: auc is greater than 0.7 = 0.72

#Step16: Use the ROC table to find the cutoff - you can use different weightages for sens and 
#spec based on the business scenario

#Step17: Check Concordance and discordance

#EDA on Logistic Regression - Multi Logistic
#Customer Churn DataSet

#Step1: Import data. Check if your Y is cat/cont. If it is cont, linear reg, cat => logistic
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
dim(customer_churn)
str(customer_churn)
summary(customer_churn)
#y variable = PaperlessBilling

#Step2: check for missing values
sum(is.na(customer_churn$SeniorCitizen))
sum(is.na(customer_churn$tenure))
sum(is.na(customer_churn$MonthlyCharges))
sum(is.na(customer_churn$TotalCharges))
#Note: Cancelling the Total Charges column as it has missing values


#Step3: Check for Outliers and remove them
ggplot(data = customer_churn, aes(x=MonthlyCharges)) + stat_boxplot(geom = "errorbar") + geom_boxplot()
ggplot(data = customer_churn,aes(x=tenure)) +stat_boxplot(geom = "errorbar") + geom_boxplot()
#if there are outliers then execute the below
mean_ten<- mean(customer_churn$MonthlyCharges)
sd_ten<- sd(customer_churn$MonthlyCharges)
ul_ten<- mean_ten+2*sd_ten

mean_ten1<- mean(customer_churn$tenure)
sd_ten1<- sd(customer_churn$tenure)
ul_ten1<- mean_ten1+2*sd_ten1


#Step4: Check SD of every X1, X2, X3, X4 . if SD ~ 0 , then drop the feature
sd(customer_churn$MonthlyCharges)
sd(customer_churn$tenure)

#Step5: Var significance --> Chisq test if p>0.05 , then drop the feature

#Step6: Var significance --> IV ( WOE )  if IV < 0.1 , then drop the feature

#Step7: Shortlist features based on steps 2,3,4,5,6
#2,3,4,5,6 executed

#Step8: Check multicollinearity using VIF

#Step9: Drop features with VIF > 5 one by one to arrive at the final list of features


#Step10: Split the data into train and test
sample.split(customer_churn$PaperlessBilling, SplitRatio = 0.65)-> split_tag2
subset(customer_churn, split_tag2==T)-> train2
subset(customer_churn, split_tag2==F)-> test2

#Step11: Model fitting/training
lm(PaperlessBilling~MonthlyCharges+tenure+Churn, data= train2, family="binomial")-> model2
predict(model2, newdata = test2, type = "response")-> result2

#Step12: Check for p -values and further drop insignificant features with p > .05
summary(model2)
# p values is less than 0.05 hence no need to drop the insignificant feature


#Step13: Fit the model again
# no need to fit the model in this case as we did not drop any feature

#Step14: Model Validation - Acc, sens , spec
#need to build a confusion matrix and get the accuracy
prediction(result2, test2$PaperlessBilling)-> pred2
performance(pred2, "acc") -> acc2
plot(acc2)
table(test2$PaperlessBilling, result2>0.74)
#Finding Accuracy
((1005+0)/(1005+0+0+1460))*100
#Accuracy=40.77%

#Step15: Need to check the cutoff - ROC table and ROC Curve. Make sure your AUC is > 0.7
performance(pred2, "auc")-> auc2
View(auc2)
#Note: auc is greater than 0.7 = 0.74

#Step16: Use the ROC table to find the cutoff - you can use different weightages for sens and 
#spec based on the business scenario

#Step17: Check Concordance and discordance












