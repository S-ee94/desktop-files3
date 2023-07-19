
rm(list=ls()) # removes all variables stored previously
install.packages("Hmisc")# import 
library(Hmisc)
data <- read.csv("C:/Users/seema/OneDrive/Desktop/COURSE/Intellipaat Data Science Live Courses/DataScience with R Programming/CSV Files/COVID19_line_list_data.csv")
describe(data) # Hmisc command 
View(data)

#cleaned up death column
# if death variable isn't 0, then the patient has died
data$death_dummy <- as.integer(data$death != 0)
unique(data$death_dummy)
View(data)

# death rate of our dataset
sum(data$death_dummy)/nrow(data)

#AGE
# Claim: people who die are older 

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#Is this statistically significant ?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
# confidence level can be changed 
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject the null hypothesis 
# here p-value~0 , so we reject the null hypothesis & conclude that 
# this is statistically significant 


#GENDER
# Claim: gender has no effect 

men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # 8.4%
mean(women$death_dummy, na.rm = TRUE) # 3.7%

#Is this statistically significant ?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)

#99% confidence: mean have from 0.8% to 8.8% higher chances of dying 
#p-values = 0.002 < 0.05, so this is statistically significant 

# full project explanation link - https://towardsdatascience.com/r-tutorial-analyzing-covid-19-data-12670cd664d6






