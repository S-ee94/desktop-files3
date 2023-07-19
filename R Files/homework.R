#R bind homework 

c1<- c("a","b","c","d","e")
c2<-c("e","f","g","h")
c3<-c("e","f","g","h")
c4<-c("j","v","k","s")
df1<-data.frame(c1)
View(df1)

c1<- c("a","b","c","d")
c2<-c("e","f","g","h")
c3<-c("e","f","g","h")
c5<-c("a","b","c","d")
df2<-data.frame(c1,c2)
View(df2)

rbind(df1,df2)->combined_data
#answer: rbind(df1,df2)
#Error in rbind(deparse.level, ...) : 
#numbers of columns of arguments do not match

#C bind homework

cbind(df1,df2)->combined_data_cbind
#answer: cbind(df1,df2)
#Error in data.frame(..., check.names = FALSE) : 
#arguments imply differing number of rows: 5, 4

#joins home work

library(dplyr)
d1<-data.frame(x=c(1:4),y=c(10:13))
d2<-data.frame(x=c(1,2,3,5),z=c(15:18))
inner_join(d1,d2,by=("x"="x"))
left_join(d1,d2,by=("x"="x"))
right_join(d1,d2,by=("x"="x"))
full_join(d1,d2,by=("x"="x"))
merge(d1,d2,by="x")

#select home work
temp_pd<-select(pd,starts_with('c')&ends_with('y'))
View(temp_pd)
temp_pd<-select(pd,starts_with('c')|starts_with("f"))
temp_pd = filter(pd, state=='CA' & total>10000)

my_data$dc <- data.frame(ifelse(is.na(data.frame(my_data$`DEPOSIT AMT`)), "D", "C"))

my_data = mutate(my_data,flag = ifelse(is.na(`WITHDRAWAL AMT`) == TRUE,"Credit","Debit"))

bank= mutate(bank_data, flag = ifelse(is.na('WITHDRAWAL.AMT')== TRUE, "Credit", "Debit"))
View(bank)

bank

table(bank$CHQ.NO.)
bank_x_Data<- mutate(bank_data, flag = ifelse(is.na("WITHDRAWAL.AMT")==TRUE,"Credit", "Debit"))
View(bank_x_Data)

#Sir code
library("readxl")


getwd()

list.files()
# xlsx files
my_data <- read_excel("bank_excel.xlsx")

my_data = mutate(my_data,flag = ifelse(is.na(`WITHDRAWAL AMT`) == TRUE,"C","D"))
View(my_data)

# Loading
library("readxl")


getwd()

list.files()
# xlsx files
my_data <- read_excel("bank.xlsx")

my_data = mutate(my_data,flag = ifelse(is.na(`WITHDRAWAL AMT`) == TRUE,"C","D"))
my_data = rename(my_data, BALANCE_AMT = `BALANCE AMT`)
my_data = rename(my_data, WITHDRAWAL_AMT = `WITHDRAWAL AMT`)
my_data = rename(my_data, DEPOSIT_AMT = `DEPOSIT AMT`)


my_data_C = filter(my_data,flag == 'C')

hist(my_data_C$DEPOSIT_AMT)


my_data_D = filter(my_data,flag == 'D')
hist(my_data_D$WITHDRAWAL_AMT)

mean_c = mean(my_data_C$DEPOSIT_AMT)
sd_c = sd(my_data_C$DEPOSIT_AMT)
ul_c = mean_c + 3 * sd_c

sus_c = filter (my_data_C ,DEPOSIT_AMT > ul_c )


mean_d = mean(my_data_D$WITHDRAWAL_AMT)
sd_d = sd(my_data_D$WITHDRAWAL_AMT)
ul_d = mean_d + 3 * sd_d

sus_d = filter (my_data_D ,WITHDRAWAL_AMT > ul_d )


min(my_data_C$DEPOSIT_AMT)
max(my_data_C$DEPOSIT_AMT)
quantile(my_data_C$DEPOSIT_AMT, 0.90)
quantile(my_data_C$DEPOSIT_AMT, 0)
quantile(my_data_C$DEPOSIT_AMT, 1)

quantile(my_data_C$DEPOSIT_AMT, 0.50)

#EDA Homework

#download this https://www.kaggle.com/mirichoi0218/insurance - home work
# age,gender, BMI, smoker  - try all steps 
#Steps
#Firstly, install the necessary packages
# Stats and dplyr for data exploration and data manipulation 
library(stats)
library(dplyr)
mydata = read.csv("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science/DataScience with R Programming/insurance.csv")
View(mydata)
str(mydata) 


#Step 1 of EDA - Data Summarization
BMI<- mydata$bmi

#Central Tendencies
#Calculating MEAN
mean(BMI)

#Calculating MEDIAN
median(BMI)

#Calculating MODE
#Mode Function
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

Mode(BMI)

#Measure of Variability

#Calculating Standard Deviation (SD)
sd(BMI)

#Calculating Variance
var(BMI)

#Calculating Inter Quantile Range (IQR)
quantile(BMI)

#End of Step 1

#Step 2 of EDA - Data Visualization
str(mydata)
Region = mydata$region

#Numeric Data - Histogram
hist(BMI)

#Categorical Data - BarPlot
table(Region)
barplot(table(Region))

#end of Step 2

#Step 3 of EDA - Data Normalization
str(mydata)

#Select all numeric variables from the data set
mydata_numeric = select(mydata, c(1,3,7))

#Normalize Data Set - Z Scores - All variables on the same scale
Zscore_mydata = scale(mydata_numeric)

#Check all the variables converted to Z Scores
View(Zscore_mydata)

#End of Step 3

#training and testing the models 
sample.split(mydata, SplitRatio = 0.70)-> split_tag
subset(mydata, split_tag==T)-> train_set
subset(mydata, split_tag==F)-> test_set
lm(region~age, train_set)-> model
summary(model)
predict(model, test_set)-> result
cbind(Actual = test_set$region, Predicted = result)-> final_result
as.data.frame(final_result)-> final_result
final_result$Actual - final_result$Predicted -> error
cbind(final_result,error)-> final_result
sqrt(mean((final_result$error)^2)) -> rmse
rmse

ggplot(data = mydata, aes(y=region, x=age)) + geom_point() + geom_smooth(method = "lm")
ggplot(data = final_result, aes(y=error, x=Predicted)) + geom_point()

qqnorm(final_result$error)

#Sir code chisquare test
library(MASS)
tbl= table(data$y, data$Age_bin)
tbl
chisq.test(tbl)

#bharath's code
lapply(c(colnames(data)),function(var)chisq.test(icu$y, icu[,var]))
for(var in c(colnames(data)){print(chisq.test(data$y,data[,var]))}
    
    
#homework
#sensitivity and specificity
#Cutoff, sensitivity and specificity

#k means clustering 

    









































