# Titanic Project - Kaggle Dataset
setwd("C:/Users/seema/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/Titanic")
getwd()
titanic_train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
tail(titanic_train)

titanic_test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)
tail(titanic_test)
str(titanic_train)
str(titanic_test)


titanic_train$IsTrainSet <- TRUE
titanic_test$IsTrainSet <- FALSE

titanic_test$Survived <- NA # adding the survived column to the test set as it exists in train set 

titanic_full <- rbind(titanic_train,titanic_test)

#Cleaning missing values 

titanic_full[titanic_full$Embarked=='', "Embarked"]<- 'S' # Replacing NA values with S for Embarked column 

table(is.na(titanic_full$Age)) # has a lot of NA Values

Age_Median <-median(titanic_full$Age, na.rm = TRUE)

titanic_full[is.na(titanic_full$Age), "Age"]<- Age_Median

table(is.na(titanic_full$Age)) #checking NA 

table(is.na(titanic_full$Fare)) # NA Value exists

Fare_Median <- median(titanic_full$Fare, na.rm = TRUE)

titanic_full[is.na(titanic_full$Fare), "Fare"]<- Fare_Median

table(is.na(titanic_full$Fare)) #checking NA 

#categorical casting
str(titanic_full)
titanic_full$Pclass <- as.factor(titanic_full$Pclass)
titanic_full$Sex <- as.factor(titanic_full$Sex)
titanic_full$Embarked <- as.factor(titanic_full$Embarked)


#splitting the titanic_full data set to train and test sets 

titanic_train <- titanic_full[titanic_full$IsTrainSet == TRUE,]
titanic_test <- titanic_full[titanic_full$IsTrainSet == FALSE,]

titanic_train$Survived<- as.factor(titanic_train$Survived)

Survived_Equation <- "Survived ~ Pclass + Sex + Age +SibSp + Parch + Fare + Embarked"
Survived_Formula <- as.formula(Survived_Equation)
library(randomForest)
titanic_model <- randomForest(formula = Survived_Formula, data = titanic_train, ntree=500, mtry=3, nodesize=0.01 * nrow(titanic_test))

Features_Equation <- "Pclass + Sex + Age +SibSp + Parch + Fare + Embarked"
Survived <-predict(titanic_model, newdata = titanic_test)


PassengerId <- titanic_test$PassengerId 
Output_df <- as.data.frame(PassengerId)
Output_df$Survived <- Survived

tail(Output_df)
write.csv(Output_df, file = "Kaggle_Submission.csv", row.names = FALSE)




