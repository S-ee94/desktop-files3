data <- read.csv("D:/DataScience/practice_datasets/UCI_Bank_Marketing/bank-additional/bank-additional/bank-additional-full.csv",header = T,
                 sep = ';'
)


cov(data$duration, data$campaign)
cov(data$duration, data$pdays)

sd(data$campaign)
var(data$campaign)


cor(data$duration, data$campaign)
cor(data$duration, data$pdays)

library(party)
library(dplyr) 
library(sqldf)
library(plotly)
library(rpivotTable)

data$y <- (data$y == "yes")*1


tail(data,100)
str(data)

summary(data)

# cf1 <- cforest(y ~ . , data= data, control=cforest_unbiased(mtry=2,ntree=50))
# varimp_x <- varimp(cf1)  # get variable importance, based on mean decrease in accuracy
# sort(varimp_x)
# 
# age            job        marital      education        default        housing 
# 0.000490       0.000214       0.000019       0.000265       0.000489      -0.000066 
# loan        contact          month    day_of_week       duration       campaign 
# 0.000032       0.003288       0.007086       0.000591       0.024272       0.000338 
# pdays       previous       poutcome   emp.var.rate cons.price.idx  cons.conf.idx 
# 0.002634       0.000759       0.002832       0.010391       0.005948       0.004848 
# euribor3m    nr.employed 
# 0.009906       0.011574 
# 
# 
# sort(varimp_x)
# class(varimp_x)
# varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
# varimpAUC(cf1)    # more robust towards class imbalance.
# 
# age            job        marital      education        default        housing 
# 0.0004441      0.0003055      0.0000024      0.0002072      0.0005256     -0.0000792 
# loan        contact          month    day_of_week       duration       campaign 
# 0.0000133      0.0033252      0.0069958      0.0005229      0.0241906      0.0003717 
# pdays       previous       poutcome   emp.var.rate cons.price.idx  cons.conf.idx 
# 0.0025518      0.0008365      0.0028316      0.0103661      0.0059093      0.0049936 
# euribor3m    nr.employed 
# 0.0099407      0.0114494 
# 


hist(data$age)

sort(unique(data$pdays))



temp <- sqldf("select  count(y) as cnt,pdays
               from data 
               group by pdays order by pdays")


rpivotTable(data, rows="education", col="y" , aggregatorName="Count", 
                       vals="mpg")


## Check Missing Values
colnames(data)
sum(is.na(data$nr.employed))

sapply(data, function(x) sum(is.na(x)))

apply(data, 2, function(x) sum(is.na(x)))

##treat missing values
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)



## Outliers

# Capping Skewed Values
# Once if the data is found to be skewed the upper and lower boundary can be capped with
# quintiles, standard deviation, etc. according to the business needs.
# >m<-mean(X21, na.rm = TRUE) # Mean of the variable X21
# >sd<-sd(X21, na.rm = TRUE) #StdDev of the variable X21
# >ul<-m+(2*sd) # Upper limit is calculated as Mean+2(StdDev)
# >ll<-m-(2*sd) # Lower limit is calculated as Mean-2(StdDev)
# # Capping values based on Mean & 2(StdDev)measures
# >X21<-as.numeric(
#   ifelse(data$X21<=ll,ll,
#          ifelse(data$X21>=ul,ul,
#                 X21)))
# >hist(X21)
# 
# "

##Binning Values
# 
# Binning Values
# Sometimes it is very difficult to explain the numeric values. Hence binning such variables can
# add some business sense and also very easy to explore data.
# >summary(X4)
# age<-as.factor(
#   ifelse(X4<=30,'Young',
#          ifelse(X4<=60,'Mature',
#                 'Old')))
# summary(age)
# counts <- table(Y,age)
# counts #Table View



# Stacked bar plot

#barplot(counts,
#main=Stacked Bar Plot"
#xlab="Age", ylab="Frequency",
#col=c("yellow","green"),
#legend=rownames(counts))



boxplot(data$age)
summary(data$age)
sd(data$age)

data$Age_bin <- cut(data$age, breaks = c(15,25,35,45,55,65,100),
                    include.lowest = TRUE)

sort(unique(data$Age_bin))

data$Age_bin
#hist(data$Age_bin)

Age.freq <- table(data$Age_bin)
Age.freq
class(Age.freq)
barplot(Age.freq)
class(data)

unique(data$pcon )
data$pcon <- (data$pdays != 999)*1
str(data)
data$pcon <- as.factor(data$pcon)



data$y <- as.factor(data$y)
data <- subset(data, select=-c(age,pdays))


##Chisq test of independence
library(MASS) 
# load the MASS package 

tbl = table(data$y, data$Age_bin) 
tbl
chisq.test(tbl)




library(InformationValue)

factor_vars <- c ("job", "marital", "education", "default", "housing", "loan", "contact",
                  "month","day_of_week", "poutcome","Age_bin","pcon")


all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars))
                     , stringsAsFactors = F)  # init output dataframe

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=data[, factor_var], Y=data$y)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=data[, factor_var], Y=data$y), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ]  # sort

all_iv

for(factor_var in factor_vars){
  data[[factor_var]] <- WOE(X=data[, factor_var], Y=data$y)
}

unique(WOE(X=data[, "pcon"], Y=data$y))

##removed as suggested by low IV
data <- subset(data, select=-c(marital ,day_of_week,housing, loan))

#data <- subset(data, select=-c(poutcome))




#removed for high VIF  # dnt remove before running the glm model # remove one by one unlike IV
data <- subset(data, select=-c(euribor3m))
data <- subset(data, select=-c(emp.var.rate))
data <- subset(data, select=-c(nr.employed))

data <- subset(data, select=-c(poutcome))

data <- subset(data, select=-c(job))

euribor3m




library(caret)
intrain<-createDataPartition(y=data$y,p=0.7,list=FALSE)
train<-data[intrain,]
test<-data[-intrain,]
str(test)

#Logistic Regression
model <- glm(y ~ ., family = binomial(link = 'logit'), data = train)
summary(model)

##Check Multicolliniarity

library(car)
vif(model)




#Confusion Matrix

train$predict <- predict(model, newdata=train, type='response')


train$predict_r <- ifelse(train$predict > 0.7,1,0)


test$predict <- predict(model, newdata=test, type='response')
test$predict_r <- ifelse(test$predict > 0.7,1,0)

misClasificError_train <- mean(train$predict_r != train$y)
1-misClasificError_train



misClasificError_test <- mean(test$predict_r != test$y)
1-misClasificError_test



table(test$predict_r,test$y)
table(train$predict_r,train$y)


## ROC
##Gini  Coeffients   :   How good it classifies  
nrow(test)
table(test$y)

library(pROC)

auc_train <- auc(train$y, train$predict)
auc_train

auc_test<- auc(test$y, test$predict)
auc_test

g <- roc(y ~ predict, data = train)
g <- roc(y ~ predict, data = test)
g

plot(g) 

colnames(data)

Concordance(actuals=train$y, predictedScores= train$predict)

y ~ job + education + default + contact + month + duration + 
  campaign + previous + cons.price.idx + cons.conf.idx + euribor3m + 
  Age_bin + pcon

##Final variable list
 colnames(data)
[1] "job"            "marital"        "education"      "default"        "contact"       
[6] "month"          "day_of_week"    "duration"       "campaign"       "previous"      
[11] "cons.price.idx" "cons.conf.idx"  "euribor3m"      "y"              "Age_bin"       
[16] "pcon"

 