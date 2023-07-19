write.csv(customer_churn, file = "D:/Intellipaat/dataScience_new/MyData.csv")


library(dplyr)
customer_churn<- read.csv("D:/Intellipaat/dataScience_new/customer_churn.csv", header = TRUE ,sep=",") 

plot(customer_churn$Dependents)
plot(customer_churn$Dependents, col="red")
plot(customer_churn$Dependents, col="coral",ylab= "Count",xlab="Dependents",main="Distribution of Dependents")


plot(customer_churn$PhoneService)  
plot(customer_churn$PhoneService,col="aquamarine4")
plot(customer_churn$PhoneService,col="aquamarine4",xlab="PhoneService",main="Distribution of PhoneService")

plot(customer_churn$Contract)  
plot(customer_churn$Contract,col="palegreen4",xlab="Contract",main="Distribution of Contract")

hist(customer_churn$tenure)  
hist(customer_churn$tenure,col="olivedrab")
hist(customer_churn$tenure,col="olivedrab", breaks=5)

library(ggplot2)
ggplot(data=customer_churn,aes(x=tenure))+geom_histogram()


ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(bins = 60)

ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(fill="red", col = 'green', bins = 40)

ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+geom_histogram()











ggplot(data = customer_churn,aes(x=Dependents))+geom_bar()

ggplot(data = customer_churn,aes(x=Dependents))+geom_bar(fill="green")

ggplot(data = customer_churn,aes(x=Dependents,fill=PaymentMethod))+geom_bar()


ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection))+geom_bar(position="dodge")















ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure))+geom_point()  

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure))+geom_point(col="slateblue3")
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Partner))+geom_point()
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=InternetService))+geom_point()
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=OnlineSecurity))+geom_point()
ggplot(data = customer_churn,aes(y=TotalCharges,x=MonthlyCharges, col=InternetService))+geom_point()


ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot()  
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot(fill="yellowgreen")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot()
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot(fill="violetred4")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=PaperlessBilling))+geom_boxplot()

ggplot(data = customer_churn,aes(x=tenure,fill=InternetService))+geom_histogram() ->g1
g1+facet_grid(~InternetService)
g1

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Contract))+geom_point()+geom_smooth()-> g2
g2+facet_grid(~Contract)


ggplot(data = customer_churn,aes(x=tenure))+geom_histogram(fill="tomato3", col="mediumaquamarine")->g1
g1+labs(title = "Distribution of tenure")->g2
g2+theme(panel.background = element_rect(fill = "olivedrab3"))->g3
g3+theme(plot.background = element_rect(fill = "palegreen4"))->g4
g4+theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "peachpuff"))


  