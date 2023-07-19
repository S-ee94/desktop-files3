customer_churn<-read.csv("D:/Intellipaat/dataScience_new/customer_churn.csv")



library(caTools)

IQR(customer_churn$MonthlyCharges)

summary(customer_churn$MonthlyCharges)

summary(customer_churn$gender)

colnames(customer_churn)

sum(is.na(customer_churn$TotalCharges))
sum(is.na(customer_churn$tenure))
sum(is.na(customer_churn$Dependents))
sum(is.na(customer_churn$MonthlyCharges))
MonthlyCharges
Dependents

cov(customer_churn$MonthlyCharges,customer_churn$tenure)

cov(customer_churn$MonthlyCharges,customer_churn$TotalCharges)


cor(customer_churn$MonthlyCharges,customer_churn$tenure)


str(customer_churn)
sd(customer_churn$tenure)
var(customer_churn$tenure)

nrow(customer_churn)
sample.split(customer_churn$MonthlyCharges,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

nrow(train)
nrow(test)

lm(MonthlyCharges~tenure, data=train)-> model1

summary(model1)
predict(model1, newdata=test)->predicted_values

cbind(Actual=test$MonthlyCharges,Predicted=predicted_values)->final_data
head(final_data)

class(final_data)

as.data.frame(final_data)->final_data
class(final_data)

final_data$Actual - final_data$Predicted ->error
head(error)

cbind(final_data,error)-> final_data
head(final_data)

sqrt(mean((final_data$error)^2))->rmse1
rmse1

lm(MonthlyCharges~InternetService,data=train)-> model2

predict(model2,newdata=test)-> result

cbind(Actual=test$MonthlyCharges,Predicted=result)->final_data2
head(final_data2)

as.data.frame(final_data2)-> final_data2
(final_data2$Actual-final_data2$Predicted)->error2

cbind(final_data2,error2) -> final_data2
sqrt(mean((final_data2$error2)^2))-> rmse2
rmse2
#-----------------------------------------------------------------------------------------------
#Multiple Linear Regression
  
library(caTools) 

sample.split(customer_churn$tenure,SplitRatio = 0.65)-> split_model
subset(customer_churn, split_model==T)->train
subset(customer_churn, split_model==F)->test

nrow(train)
nrow(test)

lm(tenure~MonthlyCharges+gender+InternetService+Contract, data=train)-> mod1

summary(mod1)
predict(mod1,test)->result1

summary(mod1)


####t-value = Beta/Standard Error
##Standard Error: 
####How far our sample Betas from Zero
Calculation: https://www.youtube.com/watch?v=1oHe1a3JqHw
Intuition : https://www.youtube.com/watch?v=mP95aVtspI8


#Residual standard error: average residual in units of dependent variable
cbind(Actual=test$tenure,Predicted=result1)->final_data1
head(final_data1)
class(final_data1)

as.data.frame(final_data1)->final_data1
class(final_data1)
final_data1$Actual - final_data1$Predicted ->error1
head(error1)
cbind(final_data1,error1)-> final_data1
head(final_data1)

sqrt(mean((final_data1$error1)^2))->rmse1
rmse1

lm(tenure~Partner+PhoneService+TotalCharges+PaymentMethod,data=train)-> mod2

summary(mod2)


mod2$coefficients





predict(mod2,test)-> result2
cbind(,PredicteActual=test$tenured=result2)->final_data2
head(final_data2)

as.data.frame(final_data2)-> final_data2
(final_data2$Actual-final_data2$Predicted)->error2
head(error2)

cbind(final_data2,error2) -> final_data2
sqrt(mean((final_data2$error2)^2,na.rm=T))->rmse2
rmse2

#Assumptions
  
library(ggplot2)  

ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()
ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()+geom_smooth(method = "lm")

lm(TotalCharges~tenure, data = customer_churn)->mod1

summary(mod1)
predict(mod1,data=customer_churn)-> result1
cbind(Actual=customer_churn$TotalCharges, Predicted=result1)-> final_data1

head(final_data1)
as.data.frame(final_data1)->final_data1
final_data1$Actual -final_data1$Predicted  -> error1
cbind(final_data1,error1)-> final_data1

ggplot(data= final_data1, aes(x=Predicted, y=error1)) + geom_point()

qqnorm(final_data1$error1)

hist(final_data1$error1)

#shapiro.test(final_data1$error1)
# 
#nrow(final_data1)
# 
#ks.test(final_data1, "pnorm", 1, 2)




#library(nortest)
#ad.test(final_data1$error1)

#plot(customer_churn$tenure, customer_churn$TotalCharges)








####Formulae

#R2 = 1 - (SSR/SST)

#Adj_R2 = R2 - [k(1-R2)/(n-k-1)]

#Here, n represents the number of observations and k represents the number of parameters.

#Assumptions:
  

#Independence - This assumes that the errors of the response variables are 
#uncorrelated with each other.
plot(final_data1$error1)

###The residuals must have constant variance
#Homoscedasticity - In homoscedasticity, the response variables possess 
#common variances in their errors. This is completely independent of the 
#value of predictor variables. 
#However, in practice, this does not hold valid as the errors can 
#be heteroscedastic. This happens when the response variable varies over 
#a wider scale.
plot(final_data1$Predicted, final_data1$error1)
plot(final_data1$Predicted, final_data1$error1)

plot(final_data1$error1)

##errors must be normally distributed
hist(final_data1$error1)
ks.test(final_data1$error1,"pnorm")
hist(final_data2$error2)
ks.test(final_data2$error2,"pnorm")
www.youtube.com/watch?v=cltWQsmBg0k

www.youtube.com/watch?v=Wk8tyPXqmI0

##What to do if errors not normally dist and heteroskedastic
https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd453.htm


#####A Standard Error is the measurement of the standard deviation 
#of the sample population from its mean. 
##std_error = sqrt(MSE)

#F-Stat = MSR/MSE
##Where MSR is the mean squared regression:

#Fstat = (ExplainedVar/(k-1))/(UnexplainedVar/(n-k))

#Fstat = (R2/(k-1))/(1-R2)/(n-k)
#n = ( Total number of observations )   
#k = ( no of variables + 1 for intercept )

(0.7286/(6)) / ((1-0.7286) / (4559))
 b#6 and 4559 are the DFs


