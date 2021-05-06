#ch 17.6 Kruskal wallis test 
#practice
#set up libraries and import dataset

library(tidyverse)

Milk_Prod0 <- read.csv('D:/Docker/Hawkes_Rcodes/Milk_Schedule_Data.csv', header = T)

#change data set to long form
#put all categories in one column
#their corresponding prices should go in another column

Milk_Prod1 <- Milk_Prod0 %>%
  gather('Schedule.1', 'Schedule.2', 'Schedule.3', 'Schedule.4', key ="Schedule", value = "Gallons") %>%
  filter(!is.na(Gallons))

#test variable 
#Milk_Prod1

#get ranks
Milk_Prod2 <- Milk_Prod1 %>%
  mutate(
    Ranked_Gallons = rank(Gallons)
  ) %>%
  group_by(Schedule) %>%
  summarise(sum(Ranked_Gallons))

Milk_Prod3 <- as.data.frame(Milk_Prod2) %>%
  rename('Ranked_Gallons' = 'sum(Ranked_Gallons)')

Milk_Prod3

#     Schedule Ranked_Gallons
# 1 Schedule.1             54
# 2 Schedule.2             42
# 3 Schedule.3             65
# 4 Schedule.4             49

#assign R values
R1 <- 54
R2 <- 42
R3 <- 65
R4 <- 49

#get how many times each categorical variable shows up
Milk_Prod_tbl <- as.data.frame(table(Milk_Prod1$Schedule)) %>%
  rename('Schedule' = 'Var1')

Milk_Prod_tbl

#     Schedule Freq
# 1 Schedule.1    5
# 2 Schedule.2    5
# 3 Schedule.3    5
# 4 Schedule.4    5

N <- length(Milk_Prod1$Schedule)
N

#assign the variables to respective variables
n1 <- 5
R1
n2 <- 5
R2
n3 <- 5
R3
n4 <- 5
R4

H <- (12/(N*(N+1))) * ((R1^2/n1)+(R2^2/n2)+(R3^2/n3)+(R4^2/n4)) -(3*(N+1))
H

#the test statistic is:
#7.868571
#the df = k - 1
#k = 3 (number of categories)
#a is 0.01
#the critical value (chi^2 (3-1),0.01) is 9.210
#this test statistic (4.16) < (9.210) critical value
#if test stat is greater than or equal to critical value, reject null hyp
#if test stat is less than critical value, fail to reject null hyp

