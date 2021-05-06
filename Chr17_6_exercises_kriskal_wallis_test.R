#ch 17.6 Kruskal wallis test 
#set up libraries and import dataset

library(tidyverse)

selling_price0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch17_kruskal_wallis_practice.csv', header = T)

#change data set to long form
#put all categories in one column
#their corresponding prices should go in another column

selling_price1 <- selling_price0 %>%
  gather('Mouse.Creek', 'Spring.Valley', 'Smyrna', key ="towns", value = "home_prices") %>%
  filter(!is.na(home_prices))

#test variable 
#selling_price1

#get ranks
selling_price2 <- selling_price1 %>%
  mutate(
    Ranked_price = rank(home_prices)
  ) %>%
  group_by(towns) %>%
  summarise(sum(Ranked_price))

selling_price3 <- as.data.frame(selling_price2) %>%
  rename('Sum_Ranked_Price' = 'sum(Ranked_price)',
         'Towns' = 'towns')

selling_price3

#           Towns       Sum_Ranked_Price
# 1   Mouse.Creek               32
# 2        Smyrna               40
# 3 Spring.Valley               64

#assign R values
R1 <- 32
R2 <- 40
R3 <- 64

selling_price_tbl <- as.data.frame(table(selling_price1$towns)) %>%
  rename('Towns' = 'Var1')

selling_price_tbl

#      Towns       Freq
# 1   Mouse.Creek    5
# 2        Smyrna    5
# 3 Spring.Valley    6

N <- length(selling_price1$home_prices)
N

#assign the variables to respective variables
n1 <- 5
R1
n2 <- 5
R2
n3 <- 6
R3

H <- (12/(N*(N+1))) * ((R1^2/n1)+(R2^2/n2)+(R3^2/n3)) -(3*(N+1))
H

#the test statistic is:
#2.270588
#the df = k - 1
#k = 3 (number of categories)
#a is 0.05
#the critical value (chi^2 (3-1),0.05) is 5.991
#this test statistic (2.271) < (5.991) critical value
#if test stat is greater than critical value, fail to reject null hyp
#if test stat is less or equal to critical value, reject null hyp

