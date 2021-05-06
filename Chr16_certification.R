#Chr16 Certification
library(tidyverse)
library(qwraps2)
library(MASS)

#Practice Q1
Calls <- c(4,21,23,17,15)
pCalls <- c(1/5,1/5,1/5,1/5, 1/5)

Calls_res <- chisq.test(Calls, p = pCalls)

print(Calls_res, digits = 8)
qchisq(0.01, 4, lower.tail = F)
###########################################

#Practice Q2
Candy <- c(38,64,84,143,61,69)
pCandy <- c(0.40,0.10,0.10,0.10,0.10,0.20)

Candy_res <- chisq.test(Candy, p = pCandy)

print(Candy_res, digits = 8)
qchisq(0.01, 5, lower.tail = F)

###########################################

#Practice Q3
#make matrix
Q3C_data0 <- matrix(c(105,46,57,
                      35,18,18),
                    ncol = 3, byrow = T)

colnames(Q3C_data0) <- c('Avg','Below_Avg','Above_Avg')
rownames(Q3C_data0) <- c('On_campus','Off_campus')
Q3C_data0 <- as.table(Q3C_data0)
Q3C_data0

#chi sq test
prod_chisq <- chisq.test(Q3C_data0)
print(prod_chisq, digits = 8)

#correct answer:
qchisq(0.05, 2, lower.tail = F)

#########################################################
#Practice Q4

sample_std <- sd(c(3.528,3.652,4.058,4.238,4.218,4.038,4.447,4.139,4.268,4.409))
n <- 10
pop_sd <- 0.3

chi_sq_value <- ((n-1)*(sample_std^2))/(pop_sd^2)
chi_sq_value

qchisq(0.01, 8, lower.tail = F)

#########################################################
#Certification Q1
Calls <- c(31,22,19,21,19)
pCalls <- c(1/5,1/5,1/5,1/5,1/5)

Calls_res <- chisq.test(Calls, p = pCalls)

print(Calls_res, digits = 8)
qchisq(0.01, 4, lower.tail = F)

#########################################################

#Certification Q2

sample_std <- sd(c(3.523,3.646,4.052,4.232,4.215,4.032,4.441,4.133))
n <- 8
pop_sd <- 0.2

chi_sq_value <- ((n-1)*(sample_std^2))/(pop_sd^2)
chi_sq_value

qchisq(0.01, 8, lower.tail = F)

#########################################################

#Certification Q3

#make matrix
Q3C_data0 <- matrix(c(105,54,48,
                      35,16,17),
                    ncol = 3, byrow = T)

colnames(Q3C_data0) <- c('Avg','Below_Avg','Above_Avg')
rownames(Q3C_data0) <- c('On_campus','Off_campus')
Q3C_data0 <- as.table(Q3C_data0)
Q3C_data0

#chi sq test
prod_chisq <- chisq.test(Q3C_data0)
print(prod_chisq, digits = 8)

#correct answer:
qchisq(0.05, 2, lower.tail = F)

############################################

#Certification Q4
Candy <- c(27,88,60,62,27,87)
pCandy <- c(0.1,0.20,0.20,0.10,0.20,0.20)

Candy_res <- chisq.test(Candy, p = pCandy)

print(Candy_res, digits = 8)
qchisq(0.01, 4, lower.tail = F)
