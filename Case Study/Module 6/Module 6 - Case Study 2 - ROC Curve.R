#Question1
#a,b,c,d,e
sample.split(customer_churn$Churn, SplitRatio = 0.80)-> split_tag1
subset(customer_churn, split_tag1==T)-> train1
subset(customer_churn, split_tag1==F)-> test1
glm(as.factor(Churn)~MonthlyCharges+tenure+TechSupport, data = train1, family = "binomial")-> log_mod_roc
predict(log_mod_roc, newdata = test1, type = "response")-> result_log_roc
prediction(result_log_roc, test1$Churn)-> pred1
performance(pred1, "acc")-> acc1
plot(acc1)
performance(pred1, "auc")-> auc1
View(auc1)
