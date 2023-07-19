#Question1
#a, b
read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/customer_churn.csv", stringsAsFactors = TRUE)-> customer_churn
View(customer_churn)
select(customer_churn,MonthlyCharges,tenure,TotalCharges)-> customer_features
View(customer_features)
na.omit(customer_features)-> customer_features
sum(is.na(customer_features))
kmeans(customer_features,3)-> k_monthly
k_monthly
cbind(CustomFeatures=customer_features, Clusters=k_monthly$cluster) ->month_group
View(month_group)
as.data.frame(month_group)-> month_group
month_group%>% filter(Clusters==1) -> month_group_1
month_group%>% filter(Clusters==2) -> month_group_2
month_group%>% filter(Clusters==3) -> month_group_3
View(month_group_1)
View(month_group_2)
View(month_group_3)
