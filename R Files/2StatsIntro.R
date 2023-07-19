mtcars

hist(mtcars$mpg)
mean(mtcars$mpg)

summary(mtcars$mpg)
sd(mtcars$mpg)

cov(mtcars$mpg,mtcars$cyl)

cor(mtcars$mpg,mtcars$cyl)

http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r


##Student's T.Test
#One Sample T.Test
set.seed(123) 
####Assuming pop is a normal distribution
bulb <- rnorm(30, mean = 9.99, sd = 0.04)
head(bulb)

# H0 : mu = 10
t.test(bulb, mu = 10,conf.level = 0.95)	
##Cannot reject null hypothesis

###Two Sample Paired t.test

set.seed(123)
# sales before the program
sales_before <- rnorm(7, mean = 50000, sd = 50)
# sales after the program.This has higher mean
sales_after <- rnorm(7, mean = 50075, sd = 50)
# draw the distribution
t.test(sales_before, sales_after,var.equal = TRUE)
#t.test(sales_before, sales_after,var.equal = TRUE,mu = 5,paired = True)

###Derivation
https://math.stackexchange.com/questions/474733/derivation-of-the-density-function-of-student-t-distribution-from-this-big-integ



####Z - Test

#To test the hypothesis that the mean systolic boold pressures of healthy subjects (status=0) and subjects with hypertention (status=1) are equal (d0=0) with known standard deviation of ??1=10 and ??2=12. The dataset 
#contains n1=25 subjects with status 0 and n2=30 with status 1.
#Blood_pressure dataset
no <- seq(1:55)
status <- c(rep(0, 25), rep(1, 30))
mmhg <- c(120,115,94,118,111,102,102,131,104,107,115,139,115,113,114,105,
          115,134,109,109,93,118,109,106,125,150,142,119,127,141,149,144,
          142,149,161,143,140,148,149,141,146,159,152,135,134,161,130,125,
          141,148,153,145,137,147,169)
blood_pressure <-data.frame(no,status,mmhg)

# Set difference to be tested
d0<-0
# Set standard deviation of sample with status 0
sigma0<-10
# Set standard deviation of sample with status 1
sigma1<-12

# Calculate the two means 
mean_status0<-mean(blood_pressure$mmhg[blood_pressure$status==0])
mean_status1<-mean(blood_pressure$mmhg[blood_pressure$status==1])

# Calculate both the sample sizes 
n_status0<-length(blood_pressure$mmhg[blood_pressure$status==0])
n_status1<-length(blood_pressure$mmhg[blood_pressure$status==1])

# Calculate test statistic and two-sided p-value
z<-((mean_status0-mean_status1)-d0)/sqrt(sigma0^2/n_status0+sigma1^2/n_status1)
p_value=2*pnorm(-abs(z))

# Output results
z
p_value
