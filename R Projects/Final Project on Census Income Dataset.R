#Importing the dataset

read.csv("C:/Users/seema/OneDrive/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/census-income.csv")-> censusData
View(censusData)
str(censusData)
#Note: all the data classes must be chr and not factor as we should perform data pre-processsing

#Tasks to be done
#Question1
#Data Pre-Processing
# a)	Replace all the missing values with NA.
censusData[censusData==" ?"]<- NA  
View(censusData)
sum(is.na(censusData))

# b)	Remove all the rows that contain NA values.
censusData<-na.omit(censusData)
sum(is.na(censusData))

# c)	Remove all whitespaces from the columns.
library(stringr) 
library(dplyr)

censusData<-censusData %>% mutate_if(is.character, str_trim)
str(censusData)

#converting the chr class to factors 
str(censusData)
censusData$workclass<-as.factor(censusData$workclass)
censusData$occupation<-as.factor(censusData$occupation)
censusData$native.country<-as.factor(censusData$native.country)
censusData$education<-as.factor(censusData$education)

censusData$marital.status<-as.factor(censusData$marital.status)
censusData$relationship<-as.factor(censusData$relationship)
censusData$race<-as.factor(censusData$race)
censusData$sex<-as.factor(censusData$sex)
str(censusData)

#Question2
#Data Manipulation

# a) Extract the "education" column and store it in "census_ed" .
census_ed<-censusData$education
View(census_ed)
head(census_ed)
#or we can also use select function
census_ed<- select(censusData,education)


# b)	Extract all the columns from "age" to "relationship" and store it in "census_seq".
census_seq<-censusData %>% select(age:relationship)
View(census_seq)
#or using only select function
census_seq<- select(censusData, age:relationship)


# c)	Extract the column number "5", "8", "11" and store it in "census_col".
census_col<-censusData[,c(5,8,11)]
View(census_col)
head(census_col)
#or using select function
census_col<- select(censusData,5,8,11)

# d)	Extract all the male employees who work in state-gov and store it in "male_gov".
str(censusData)
male_gov<-censusData %>% filter(sex == "Male"& workclass=="State-gov")
View(male_gov)

# e)	Extract all the 39 year olds who either have a bachelor's degree 
# or who are native of United States and store the result in "census_us".
table(censusData$native.country)
table(censusData$education)
census_us<-censusData %>% filter(age==39&(education=="Bachelors"|native.country=="United-States"))
View(census_us)

# f)	Extract 200 random rows from the "census" data frame and store it in "census_200".
census_200<-sample_n(censusData,200)
View(census_200)

# g)	Get the count of different levels of the "workclass" column.
countWcls<-count(censusData,workclass)
countWcls
#or use table function
table(censusData$workclass)

# h)	Calculate the mean of "capital.gain" column grouped according to "workclass".
censusData %>% group_by(workclass) %>% summarise(mean(capital.gain))

#Question3
#Data Visualization

library(ggplot2)
# a)	Build a bar-plot for the "relationship" column and fill the bars according to the "race" column.
ggplot(censusData,aes(x=relationship,fill=race))+ geom_bar()

# i.	Set x-axis label to 'Categories of Relationships'
# ii.	Set y-axis label to 'Count of Categories'
ggplot(censusData,aes(x=relationship,fill=race))+
  geom_bar()+
  labs(x="Categories of Relationships",y="Count of Categories")

# iii.	Fill the bars according to "sex"
ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar()+
  labs(x="Categories of Relationships",y="Count of Categories")
# iv.	Set the position of the bars to "dodge"
ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories")
# v.	Set the title of plot to be 'Distribution of Relationships by Sex"
ggplot(censusData,aes(x=relationship,fill=sex))+
  geom_bar(position = "dodge")+
  labs(x="Categories of Relationships",y="Count of Categories",title = "Distribution of Relationships by Sex")

# b)	Build a Histogram for the "age" column with number of bins equal to 50.
ggplot(censusData,aes(x=age))+geom_histogram(bins = 50)

# i)	Fill the bars of the histogram according to yearly income column i.e., "X"
ggplot(censusData,aes(x=age,fill=X))+geom_histogram(bins = 50)

# ii)	Set the title of the plot to "Distribution of Age".
ggplot(censusData,aes(x=age,fill=X))+geom_histogram(bins = 50)+
  labs(title = "Distribution of Age")

# iii)Set the legend title to "Yearly income".
ggplot(censusData,aes(x=age,fill=X))+geom_histogram(bins = 50)+
  labs(title = "Distribution of Age",col='Yearly income')

# iv) Set the theme of the plot to black and white.
ggplot(censusData,aes(x=age))+geom_histogram(bins = 50)+
  labs(title = "Distribution of Age")+theme_bw()


# c)	Build a scatter-plot between "capital.gain" and "hours.per.week".
#     Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.
ggplot(censusData,aes(x=capital.gain,y=hours.per.week))+geom_point()
# i)	Set the transparency of the points to 40% and size as 2.
ggplot(censusData,aes(x=capital.gain,y=hours.per.week))+geom_point(alpha=0.6,size=2)

# ii)	Set the color of the points according to the "X" (yearly income) column. 
ggplot(censusData,aes(x=capital.gain,y=hours.per.week,col=X))+geom_point(alpha=0.6,size=2)

# iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title
# to "Capital Gain vs Hours per Week by Income", and legend label to "Yearly Income".
ggplot(censusData,aes(x=capital.gain,y=hours.per.week,fill=X))+
  geom_point(alpha=0.6,size=2)+labs(x="Capital Gain",y="Hours per Week",title = "Capital Gain vs Hours per Week by Income", col="Yearly Income") 

# d)	Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
# "age" on the y-axis.
ggplot(censusData,aes(x=education,y=age))+geom_boxplot()

# i)	Fill the box-plots according to the "sex" column.
ggplot(censusData,aes(x=education,y=age,fill=sex))+geom_boxplot()

# ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(censusData,aes(x=education,y=age,fill=sex))+geom_boxplot()+labs(title = "Box-Plot of age by Education and Sex") 

#Question4
#Linear Regression

# a)	Build a simple linear regression model as follows:

# i)	Divide the dataset into training and test sets in 70:30 ratio.

library("caTools")
split_data<-sample.split(censusData$hours.per.week,SplitRatio = 0.70)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)

# ii)	Build a linear model on the test set where the dependent variable is
# "hours.per.week" and independent variable is "education.num".
str(censusData)
LR_model<-lm(hours.per.week~education.num,data=censusTrain)
summary(LR_model)

# iii)	Predict the values on the train set and find the error in prediction. and 
#iv)Find the root-mean-square error (RMSE).
censusP<-predict(LR_model,newdata=censusTest)
head(censusP)
censusD<-cbind(Actual=censusTest$hours.per.week,Predicted=censusP)
View(censusD)
censusD<-as.data.frame(censusD)
censusE<-censusD$Actual-censusD$Predicted
View(censusE)
censusD<-cbind(censusD,censusE)
sqrt(mean((censusD$censusE)^2))-> rmse
rmse

#Question5
#Logistic Regression

# a)	Build a simple logistic regression model as follows:

# i)	Divide the dataset into training and test sets in 65:35 ratio.
split_data1<-sample.split(censusData$X,SplitRatio = 0.65)
censusTrain1<-subset(censusData,split_data1==T)
censusTest1<-subset(censusData,split_data1==F)
nrow(censusTrain1)
nrow(censusTest1)

# ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent 
#variable is "occupation".
log_mod<-glm(X~occupation,data=censusTrain1,family = "binomial")
summary(log_mod)

# iii)	Predict the values on the test set.
pred_val<-predict(log_mod,newdata =censusTest1,type = "response")
head(pred_val)
range(pred_val)
library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest1$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)## Check for which valve accuracy get constant
table(censusData$X)
 
# iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off.
lm.pred<-ifelse(pred_val>0.47,">50K","<=50K")  
lm.pred

# v)	Build a confusion matrix and find the accuracy.
tab<-table(lm.pred,censusTest1$X)
tab
accuracy<-sum(diag(tab))/sum(tab)
accuracy


# vi)	Plot the ROC curve and find the auc(Area Under Curve). 
roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc

split_data1<-sample.split(censusData$X,SplitRatio = 0.80)
censusTrain2<-subset(censusData,split_data1==T)
censusTest2<-subset(censusData,split_data1==F)
log_mod2<-glm(X~age+workclass+education,data=censusTrain2,family = "binomial")
summary(log_mod2)
pred_val<-predict(log_mod2,newdata =censusTest2,type = "response")
head(pred_val)

library(ROCR) ## TO decide Accuracy
predict_log_roc<-prediction(pred_val,censusTest2$X)
predict_log_roc
acc<-performance(predict_log_roc,"acc")
plot(acc)
lm.pred<-ifelse(pred_val>0.45,">50K","<=50K")  
lm.pred
censusData$X[108]
tab<-table(lm.pred,censusTest2$X)
tab
accuracy<-sum(diag(tab))/sum(tab)
accuracy

roc<-performance(predict_log_roc,"tpr","fpr")
plot(roc)
performance(predict_log_roc, "auc")->auc
auc
auc<-auc@y.values[[1]]
auc

#Question6
#Decision Tree

# a)	Build a decision tree model as follows:

# i)	Divide the dataset into training and test sets in 70:30 ratio.
split_data<-sample.split(censusData$X,SplitRatio = 0.70)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)

# ii)	Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables 
#as independent variables.
# iii)	Plot the decision tree.
# iv)	Predict the values on the test set.
# v)	Build a confusion matrix and calculate the accuracy.

library(rpart)
library(rpart.plot) 

census_model<-rpart(formula = X~.,
                    data = censusTrain,
                    method = "class")

rpart.plot(x= census_model, type= 5, extra = 0,tweak = 1.4)


class_prediction<-predict(census_model,
                          newdata = censusTest,
                          type = "class")

tab<-table(class_prediction,censusTest$X)
tab
sum(diag(tab))/sum(tab)

#Question7
#Random Forest

# a)	Build a random forest model as follows:
# 
# i)	Divide the dataset into training and test sets in 80:20 ratio.
# ii)	Build a random forest model where the dependent variable is "X"(Yearly Income) and the 
#rest of the variables as independent variables and number of trees as 300.
# iii)	Predict values on the test set
# iv)	Build a confusion matrix and calculate the accuracy

split_data<-sample.split(censusData$X,SplitRatio = 0.80)
censusTrain<-subset(censusData,split_data==T)
censusTest<-subset(censusData,split_data==F)
nrow(censusTrain)
nrow(censusTest)

library(randomForest)

census_model<-randomForest(formula=X~.,
                           data=censusTrain,ntree=300)
cenus_prediction<-predict(census_model,
                          newdata = censusTest,
                          type = "class")

tab<-table(cenus_prediction,censusTest$X)
tab
sum(diag(tab))/sum(tab)































