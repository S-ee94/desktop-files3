#Loan Approval Prediction Project

#A) Data PreProcessing 

customer_loan_details <- read.csv("C:/Users/seema/OneDrive/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/cutomer_loan.csv", sep = ",")

# Print the structure of the dataframe
str(customer_loan_details)
head(customer_loan_details)
View(customer_loan_details)

#a)  Have a glance at the structure of the dataset and find if there are any missing values present
any(is.na(customer_loan_details))

#b) Calculate the debt-to-income ratio and add it as a new column named 'dti'
customer_loan_details$dti <- (customer_loan_details$debts/customer_loan_details$income)*100
View(customer_loan_details)

#c) Create a new variable named 'loan_decision_status', where the value would be
#'0' if 'loan_decision_type' is equal to 'denied', else it would be '1'
customer_loan_details$loan_decision_status <- ifelse(customer_loan_details$loan_decision_type == 'Denied', 0, 1)
View(customer_loan_details)

#i) Convert this variable into a factor
customer_loan_details$loan_decision_status <- factor(customer_loan_details$loan_decision_status, levels = c(0, 1))

#d) Create a new data-set named 'customer_loan_refined', which would have
#these column numbers from the original dataframe - (3,4,6,7,8,11,13,14)
customer_loan_refined <- customer_loan_details[,c(3,4,6:8,11,13:14)]
head(customer_loan_refined)

#e) Encode 'gender', 'marital_status', 'occupation', and 'loan_type' as factors and 
#then convert them into numeric
customer_loan_refined$gender <- as.numeric(factor(customer_loan_refined$gender,
                                                  levels = c('Male','Female'),
                                                  labels = c(1,2)))

customer_loan_refined$marital_status <- as.numeric(factor(customer_loan_refined$marital_status,
                                                          levels = c('Divorced','Married','Single'),
                                                          labels = c(1,2,3)))

customer_loan_refined$occupation <- as.numeric(factor(customer_loan_refined$occupation,
                                                      levels = c('Accout','Business','IT','Manager','NYPD'),
                                                      labels = c(1,2,3,4,5)))

customer_loan_refined$loan_type <- as.numeric(factor(customer_loan_refined$loan_type,
                                                     levels = c('Auto','Credit','Home','Personal'),
                                                     labels = c(1,2,3,4)))

head(customer_loan_refined)

#B) Model Building

#a)  Divide the data into 'train' & 'test' sets and set the split-ratio to be 70%
library(caTools)
set.seed(123)
split = sample.split(customer_loan_refined$loan_decision_status, SplitRatio = 0.70)
training_set = subset(customer_loan_refined, split == TRUE)
test_set = subset(customer_loan_refined, split == FALSE)

#b) Apply feature scaling on all the columns of 'train' & 'test' set, except the 'loan_decision_status' column
training_set[-8] = scale(training_set[-8])
test_set[-8] = scale(test_set[-8])
head(training_set)

#c) Apply principal component analysis (PCA) on the first 7 columns of 'train' & 'test'
#set. The number of principal components obtained should be 2 

# install.packages("caret")
library(caret)
pca = preProcess(x = training_set[-8], method = 'pca', pcaComp = 2)
training_set_pca = predict(pca, training_set)
training_set_pca = training_set_pca[c(2, 3, 1)]
test_set_pca = predict(pca, test_set)
test_set_pca = test_set_pca[c(2, 3, 1)]
head(test_set_pca)

#d) Build the naïve bayes model on the train set to predict the loan
# install.packages("e1071")
library(e1071)
classifier = naiveBayes(x = training_set_pca[-3], y = training_set_pca$loan_decision_status)

#e) Predict the values on the test set 
y_pred = predict(classifier, newdata = test_set_pca[-3])

#f) Build a confusion matrix for actual values and predicted values 
confusionMatrix(table(test_set_pca[, 3], y_pred))

