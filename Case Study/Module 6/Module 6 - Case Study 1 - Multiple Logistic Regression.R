#Question1
#a,b,c,d,e
sample.split(customer_churn$gender, SplitRatio = 0.65)-> split_tag1
subset(customer_churn, split_tag1==T)-> train1
subset(customer_churn, split_tag1==F)-> test1
glm(as.factor(gender)~Dependents+InternetService+Contract, data = train1, family = "binomial")-> log_mod_multi
predict(log_mod_multi, newdata = test1, type = "response")-> result_log_multi
range(result_log_multi)
table(test1$gender, result_log_multi>0.49)
#Accuracy
(135+1092)/(135+1092+1086+152) = 0.4977

#Question2
#a,b,c,d
sample.split(customer_churn$gender, SplitRatio = 0.65)-> split_tag2
subset(customer_churn, split_tag1==T)-> train2
subset(customer_churn, split_tag1==F)-> test2
glm(gender~tenure+MonthlyCharges+PaymentMethod, data = train2, family = "binomial")-> log_mod_multi2
predict(log_mod_multi2, newdata = test2, type = "response")-> result_log_multi2
range(result_log_multi2)
table(test2$gender, result_log_multi2>0.49)
#Accuracy
(191+1052)/(191+1052+1030+192) = 0.5042



