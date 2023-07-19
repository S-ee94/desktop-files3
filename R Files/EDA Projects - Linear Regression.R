#EDA on Linear Regression - Simple Linear
#Customer Churn DataSet

#Step1: Import data. Check if your Y is cat/cont. If it is cont, linear reg, cat => logistic
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
dim(customer_churn)
str(customer_churn)
summary(customer_churn)
#y variable = MonthlyCharges

#Step2: check for missing values
sum(is.na(customer_churn$SeniorCitizen))
sum(is.na(customer_churn$tenure))
sum(is.na(customer_churn$MonthlyCharges))
sum(is.na(customer_churn$TotalCharges))

#Step3: Check for Outliers and remove them
ggplot(data = customer_churn, aes(x=tenure)) + stat_boxplot(geom = "errorbar") + geom_boxplot()
#if there are outliers then execute the below
mean_ten<- mean(customer_churn$tenure)
sd_ten<- sd(customer_churn$tenure)
ul_ten<- mean_ten+2*sd_ten

#Step4: Check for linearity -> decide on transformations 
#Simple Linear Regression y=MonthlyCharges x=tenure

#Step5: Check SD of every X1, X2, X3, X4 . if SD ~ 0 , then drop the feature
#Not required since the model is simple linear

#Step6: Variance Significance, if the p>0.05, then drop the feature
cov(customer_churn$MonthlyCharges, customer_churn$tenure)
cor(customer_churn$MonthlyCharges, customer_churn$tenure)

#Step7:  Shortlist features based on steps 2,3,4,5,6
#2,3,4,5,6 executed

#Step8: Check multicollinearity using VIF
#Not required since the model is simple linear

#Step9: Drop features with VIF > 5 one by one to arrive at the final list of features
#Not required since the model is simple linear

#Step10: Split the data into train and test
sample.split(customer_churn, SplitRatio = 0.65)-> split_tag1
subset(customer_churn, split_tag1==T)-> train1
subset(customer_churn, split_tag1==F)-> test1

#Step11: Model fitting/training
lm(MonthlyCharges~tenure, data= train1)-> model1
predict(model1, newdata = test1)-> result1
cbind(Actual1=test1, Predicted1=result1)-> final_data1
as.data.frame(final_data1)-> final_data1
head(final_data1)
final_data1$Actual1.MonthlyCharges - final_data1$Predicted1-> error1
head(error1)
cbind(final_data1,error1)-> final_data1
sqrt(mean((final_data1$error1)^2))-> rmse1
rmse1

#Step12: Check for p -values and further drop insignificant features with p > .05
summary(model1)
# p values is less than 0.05 hence no need to drop the insignificant feature

#Step13: Fit the model again
# no need to fit the model in this case as we did not drop any feature

#Step14: Model Validation - Adj R2, RMSE, MAPE
summary(model1)
mean(abs((final_data1$Actual1.MonthlyCharges-final_data1$Predicted1)/final_data1$Actual1.MonthlyCharges))*100
#rmse1 = 28.89
#Adjusted R-squared:  0.05879 
#MAPE = 64.51%

#Step15: Error Assumptions - Normal, const var, random - no pattern  
#checking the linearity
ggplot(data = customer_churn, aes(y=MonthlyCharges, x=tenure)) + geom_point() + geom_smooth(method = "lm")

#Residual v/s fit graph
lm(MonthlyCharges~tenure, data = customer_churn)-> mod1
predict(mod1, data=customer_churn)-> res1
cbind(Actual=customer_churn$MonthlyCharges,Predicted=res1)-> final_data
View(final_data)
as.data.frame(final_data)-> final_data
final_data$Actual - final_data$Predicted-> error
cbind(final_data, error)-> final_data
ggplot(data = final_data, aes(x=Predicted,y=error))+ geom_point()
#Note: if there is a pattern between predicted and error, then linear model cannot be built, this assumption fails.

#checking the normal distribution of the residuals
qqnorm(final_data$error)
qqline(final_data$error)
#Note: this gives the fit line and the distribution of the residuals
#Note: if the residuals are not normal and the distribution goes away, then this assumption fails.

#EDA on Linear Regression - Multi Linear
#Step1: Import data. Check if your Y is cat/cont. If it is cont, linear reg, cat => logistic
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
dim(customer_churn)
str(customer_churn)
summary(customer_churn)
#y variable = MonthlyCharges

#Step2: check for missing values
sum(is.na(customer_churn$SeniorCitizen))
sum(is.na(customer_churn$tenure))
sum(is.na(customer_churn$MonthlyCharges))
sum(is.na(customer_churn$TotalCharges))

#Step3: Check for Outliers and remove them
ggplot(data = customer_churn, aes(x=tenure)) + stat_boxplot(geom = "errorbar") + geom_boxplot()
ggplot(data = customer_churn, aes(x=TotalCharges)) + stat_boxplot(geom = "errorbar") + geom_boxplot()
#if there are outliers then execute the below
mean_ten<- mean(customer_churn$tenure)
sd_ten<- sd(customer_churn$tenure)
ul_ten<- mean_ten+2*sd_ten

mean_ten<- mean(customer_churn$TotalCharges)
sd_ten<- sd(customer_churn$TotalCharges)
ul_ten<- mean_ten+2*sd_ten

#Step4: Check for linearity -> decide on transformations 
#Multi Linear Regression y=MonthlyCharges x=tenure, TotalCharges, Senior Citizen, StreamingTV

#Step5: Check SD of every X1, X2, X3, X4 . if SD ~ 0 , then drop the feature
sd(customer_churn$tenure)
sd(customer_churn$TotalCharges, na.rm = TRUE)
sd(customer_churn$SeniorCitizen)
#Note: Senior Citizen sd is ~0 hence dropping this column so x=tenure, TotalCharges, StreamingTV

#Step6: Variance Significance, if the p>0.05, then drop the feature
cov(customer_churn$MonthlyCharges, customer_churn$tenure)
cov(customer_churn$MonthlyCharges, customer_churn$TotalCharges)
cor(customer_churn$MonthlyCharges, customer_churn$tenure)
cor(customer_churn$MonthlyCharges,customer_churn$TotalCharges)
#Note; Total Charges has NA so dropping it. x=tenure, StreamingTV

#Step7:  Shortlist features based on steps 2,3,4,5,6
#2,3,4,5,6 executed

#Step8: Check multicollinearity using VIF

#Step9: Drop features with VIF > 5 one by one to arrive at the final list of features
#Not required since the model is simple linear

#Step10: Split the data into train and test
sample.split(customer_churn, SplitRatio = 0.65)-> split_tag2
subset(customer_churn, split_tag2==T)-> train2
subset(customer_churn, split_tag2==F)-> test2

#Step11: Model fitting/training
lm(MonthlyCharges~tenure+StreamingTV, data= train2)-> model2
predict(model2, newdata = test2)-> result2
cbind(Actual2=test2, Predicted2=result2)-> final_data2
as.data.frame(final_data2)-> final_data2
head(final_data2)
final_data2$Actual2.MonthlyCharges - final_data2$Predicted2-> error2
head(error2)
cbind(final_data2,error2)-> final_data2
sqrt(mean((final_data2$error2)^2))-> rmse2
rmse2


#Step12: Check for p -values and further drop insignificant features with p > .05
summary(model2)
# p value is less than 0.05 hence no need to drop the insignificant feature


#Step13: Fit the model again
# no need to fit the model in this case as we did not drop any feature


#Step14: Model Validation - Adj R2, RMSE, MAPE
summary(model2)
mean(abs((final_data2$Actual2.MonthlyCharges-final_data2$Predicted2)/final_data2$Actual2.MonthlyCharges))*100
#rmse2 = 16.17
#Adjusted R-squared:  0.7198 
#MAPE = 22.91%

#Step15: Error Assumptions - Normal, const var, random - no pattern  
#checking the linearity
ggplot(data = customer_churn, aes(y=MonthlyCharges, x=tenure)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = customer_churn, aes(y=MonthlyCharges,x=StreamingTV)) +geom_point() + geom_smooth(method = "lm")


#Residual v/s fit graph
lm(MonthlyCharges~tenure+StreamingTV, data = customer_churn)-> mod2
predict(mod2, data=customer_churn)-> res2
cbind(Actual=customer_churn$MonthlyCharges,Predicted=res2)-> final_data
View(final_data)
as.data.frame(final_data)-> final_data
final_data$Actual - final_data$Predicted-> error
cbind(final_data, error)-> final_data
ggplot(data = final_data, aes(x=Predicted,y=error))+ geom_point()
#Note: if there is a pattern between predicted and error, then linear model cannot be built, this assumption fails.
#Note: there is no pattern, hence building a multilinear model here

#checking the normal distribution of the residuals
qqnorm(final_data$error)
qqline(final_data$error)
#Note: this gives the fit line and the distribution of the residuals
#Note: if the residuals are not normal and the distribution goes away, then this assumption fails.
