#ch 17.6 Kruskal wallis test 
#practice
#set up libraries and import dataset

library(tidyverse)

ISP_score0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch17_6_KW_practiceQ3.csv', header = T)

#change data set to long form
#put all categories in one column
#their corresponding prices should go in another column

ISP_score1 <- ISP_score0 %>%
  gather('Server.1', 'Server.2', 'Server.3', 'Server.4', key ="ISP", value = "Server_test_score") %>%
  filter(!is.na(Server_test_score))

#test variable 
#school_type1

#get ranks
ISP_score2 <- ISP_score1 %>%
  mutate(
    Ranked_STS = rank(Server_test_score)
  ) %>%
  group_by(ISP) %>%
  summarise(sum(Ranked_STS))

ISP_score3 <- as.data.frame(ISP_score2) %>%
  rename('Ranked_STS' = 'sum(Ranked_STS)')

ISP_score3

#       ISP     Ranked_STS
# 1 Server.1         40
# 2 Server.2         58
# 3 Server.3         61
# 4 Server.4         51

#assign R values
R1 <- 40
R2 <- 58
R3 <- 61
R4 <- 51

ISP_score_tbl <- as.data.frame(table(ISP_score1$ISP)) %>%
  rename('ISP_score' = 'Var1')

ISP_score_tbl

#   ISP_score Freq
# 1  Server.1    5
# 2  Server.2    5
# 3  Server.3    5
# 4  Server.4    5

N <- length(ISP_score1$ISP)
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
#1.491429
#the df = k - 1
#k = 3 (number of categories)
#a is 0.01
#the critical value (chi^2 (3-1),0.01) is 9.210
#this test statistic (4.16) < (9.210) critical value
#if test stat is greater than or equal to critical value, reject null hyp
#if test stat is less than critical value, fail to reject null hyp
#########################################

ISP_score0 <- read.csv('D:/Docker/Hawkes_Rcodes/Server_Test_Scores.csv', header = T)

#change data set to long form
#put all categories in one column
#their corresponding prices should go in another column

ISP_score1 <- ISP_score0 %>%
  gather('Server.1', 'Server.2', 'Server.3', 'Server.4', key ="ISP", value = "Server_test_score") %>%
  filter(!is.na(Server_test_score))

#test variable 
#school_type1

#get ranks
ISP_score2 <- ISP_score1 %>%
  mutate(
    Ranked_STS = rank(Server_test_score)
  ) %>%
  group_by(ISP) %>%
  summarise(sum(Ranked_STS))

ISP_score3 <- as.data.frame(ISP_score2) %>%
  rename('Ranked_STS' = 'sum(Ranked_STS)')

ISP_score3

#       ISP     Ranked_STS
# 1 Server.1         37
# 2 Server.2         30
# 3 Server.3         79
# 4 Server.4         64

#assign R values
R1 <- 37
R2 <- 30
R3 <- 79
R4 <- 64

ISP_score_tbl <- as.data.frame(table(ISP_score1$ISP)) %>%
  rename('ISP_score' = 'Var1')

ISP_score_tbl

#   ISP_score Freq
# 1  Server.1    5
# 2  Server.2    5
# 3  Server.3    5
# 4  Server.4    5

N <- length(ISP_score1$ISP)
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

#the critical value (chi^2 (4-1),0.01) is 11.341