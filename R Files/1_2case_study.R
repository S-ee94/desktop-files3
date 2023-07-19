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

median(my_data_C$DEPOSIT_AMT)


for(i in names(my_data))
{
  print(i)
  print(sum(is.na(my_data[,i])))
}

for(i in names(my_data_D))
{
  print(i)
  print(sum(is.na(my_data_D[,i])))
}

cor(my_data_D$WITHDRAWAL_AMT,my_data_D$BALANCE_AMT)

cor(my_data_C$DEPOSIT_AMT,my_data_C$BALANCE_AMT)
