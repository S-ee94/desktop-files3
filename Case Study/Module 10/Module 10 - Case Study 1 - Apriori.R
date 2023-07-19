#Question1
#a, b, c, d
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)
data("Groceries")
summary(Groceries)
apriori(Groceries, parameter = list(support=0.05,confidence=0.3))-> rule1
inspect(head(rule1,3))
#inspecting and sorting
inspect(head(sort(rule1,by="lift"),3))
plot(rule1, method = "grouped")

#Question2
#a, b
apriori(Groceries, parameter = list(support=0.003,confidence=0.7))-> rule2
inspect(head(rule2,5))
#inspecting and sorting
inspect(head(sort(rule2,by="lift"),3))
plot(rule2, method = "grouped")






