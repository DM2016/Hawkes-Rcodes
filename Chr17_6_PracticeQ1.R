#ch 17.6 Kruskal wallis test 
#practice
#set up libraries and import dataset

library(tidyverse)

school_type0 <- read.csv('D:/Docker/Hawkes_Rcodes/School_Scores.csv', header = T)

#change data set to long form
#put all categories in one column
#their corresponding prices should go in another column

school_type1 <- school_type0 %>%
  gather('Public.School', 'Private.School', 'Home.School', key ="School_type", value = "Student_Reading_Speed") %>%
  filter(!is.na(Student_Reading_Speed))

#test variable 
#school_type1

#get ranks
school_type2 <- school_type1 %>%
  mutate(
    Ranked_SRS = rank(Student_Reading_Speed)
  ) %>%
  group_by(School_type) %>%
  summarise(sum(Ranked_SRS))

school_type3 <- as.data.frame(school_type2) %>%
  rename('Ranked_SRS' = 'sum(Ranked_SRS)')

school_type3

#      School_type    Ranked_SRS
# 1    Home.School         42
# 2 Private.School         29
# 3  Public.School         49

#assign R values
R1 <- 42
R2 <- 29
R3 <- 49

school_type_tbl <- as.data.frame(table(school_type1$School_type)) %>%
  rename('School_type' = 'Var1')

school_type_tbl

#      School_type Freq
# 1    Home.School    5
# 2 Private.School    5
# 3  Public.School    5

N <- length(school_type1$Student_Reading_Speed)
N

#assign the variables to respective variables
n1 <- 5
R1
n2 <- 5
R2
n3 <- 5
R3

H <- (12/(N*(N+1))) * ((R1^2/n1)+(R2^2/n2)+(R3^2/n3)) -(3*(N+1))
H

#the test statistic is:
#4.16
#the df = k - 1
#k = 3 (number of categories)
#a is 0.01
#the critical value (chi^2 (3-1),0.01) is 9.210
#this test statistic (4.16) < (9.210) critical value
#if test stat is greater than or equal to critical value, reject null hyp
#if test stat is less than critical value, fail to reject null hyp

