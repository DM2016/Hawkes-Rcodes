#ch 16 practice
library(tidyverse)
library(qwraps2)
library(MASS)
library(janitor)

Lenders <- c('EB','AL','GW','GL','FS','AB','QK','CC')
APR <- c(3.521,3.645,4.051,4.231,4.211,4.031,4.441,4.132)

APR_sd <- sd(APR)
n <- 8
pop_sd <- 0.1

#chisq.test
chi_sq_value <- ((n-1)*(APR_sd^2))/(pop_sd^2)
chi_sq_value
#############################################
#Q2 chi_sq
options(digits=6)
weekly_sales <- c(23804,29916,16242,26126,19337,20698,18113,26538,29118,19065,28111,29869)
n <- 12
pop_sd <- 5300

weekly_sales_sd <- sd(weekly_sales)
chi_sq_value <- ((n-1)*(weekly_sales_sd^2))/(pop_sd^2)
chi_sq_value

#digits = getOption("qwraps2_frmt_digits", 3)

qchisq(0.025, 11, lower.tail = F)
###############################################
#generalized chi se formula
sample_std <- sd(c(3.528,3.652,4.058,4.238,4.218,4.038,4.447,4.139,4.268,4.409))
n <- 10
pop_sd <- 5500

chi_sq_value <- ((n-1)*(sample_std^2))/(pop_sd^2)
chi_sq_value

qchisq(0.01, 8, lower.tail = F)

##############################

var <- (sd(c(3.505,3.629,4.035,4.215,4.196,4.015)))^2
var
qchisq(0.005, 5, lower.tail = F)

qchisq(0.01, 8, lower.tail = F)
##############################

#get chi sq statistic for numbers and proportions

insurance1 <- c(162,56,112)
p1 <- c(0.2,0.4,0.4)
insurance2 <- c(66,132,132)
p2 <- c(0.2,0.4,0.4)

insurance_res <- chisq.test(insurance1, p = c(0.2, 0.4, 0.4))
print(insurance_res, digits = 8)

# data:  insurance1
# X-squared = 186.424, df = 2, p-value < 2.22e-16

#correct answer 186.424

qchisq(0.01, 2, lower.tail = F)
#9.21034

#186.424 > 9.21034, we reject null hyp

#get chi sq statistic for numbers and proportions

attitude1 <- c(79.8,83.6,49.4,167.2)
p1 <- c(0.21,0.22,0.13,0.44)
attitude2 <- c(57,110.2,53.2,159.6)
p2 <- c(0.15,0.29,0.14,0.42)

attitude <- data.frame(attitude1,p1,attitude2,p2)
attitude_tbl <- table(attitude$attitude1,attitude$attitude2)
attitude_res <- chisq.test(attitude1, attitude2)

#use the second (observed) set of observations, with the first (expected) set of proportions
attitude_res <- chisq.test(attitude2, p = c(0.21,0.22,0.13,0.44))
attitude_res <- chisq.test(attitude2, p = p1)

print(attitude_res, digits = 8)

qchisq(0.025, 3, lower.tail = F)

################################

FA <- c(15,11,11,10,13,18,11,8,11,13,8,10)
pA <- c(1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12)

Accidents_res <- chisq.test(FA, p = pA)

print(Accidents_res, digits = 8)
qchisq(0.025, 11, lower.tail = F)

################################
#Ch 16.2 Q1
I1 <- c(61,123,166)
pI <- c(0.3, 0.3, 0.4)

Insurance_res <- chisq.test(I1, p = pI)
print(Insurance_res, digits = 8)
qchisq(0.01, 2, lower.tail = F)

################################
#Ch 16.2 Q2
EC1 <- c(105.6,19.8,79.2,125.4)
pEC1 <- c(0.32,0.06,0.24,0.38)
EC2 <- c(82.5,29.7,92.4,125.4)
pEC2 <- c(0.25,0.09,0.28,0.38)

Energy_res <- chisq.test(EC2, p = pEC1)
print(Energy_res, digits = 8)
qchisq(0.01, 3, lower.tail = F)

################################
#Ch 16.3 practice

#get E(n favorable)

Total_Fav <- 234 #total from col 1
Total_subj <- 401 #total participants in study
Total_age18to30 <- 98 #total from row 1
Total_age45plus <- 149
Total_UnFav <- 82

#get favorable proportions of ages 18-30
p_fav <- Total_Fav/Total_subj
p18to30 <- Total_age18to30/Total_subj

En_fav <- Total_subj*p_fav*p18to30
En_fav

#get unfavorable proportions of ages 45+

p_unfav <- Total_UnFav/Total_subj
p_45plus <- Total_age45plus/Total_subj

En_unfav0 <- Total_subj*p_unfav*p_45plus
En_unfav0

#get chi sq test stat

prod_data0 <- matrix(c(66.0,57.2,16.0,20.0,16.0,20.80,
                       70.0,89.90,42.0,31.50,42.0,32.60,
                       98.0,86.90,24.0,30.50,27.0,31.60),
                     ncol = 6, byrow = T)

prod_data1 <- matrix(c(66,16,16,
                       70,42,42,
                       98,24,27),
                     ncol = 3, byrow = T)

# #x = c(66,16,16,
#        70,42,42,
#        98,24,27)
# 
# #y = c(57.2,20.0,20.8,
#       89.9,31.5,32.6,
#       86.9,30.5,31.6)

#colnames(prod_data0) <- c('n_fav', 'En_fav', 'n_unfav', 'En_unfav', 'n_Neu', 'En_Neu')
colnames(prod_data1) <- c('n_fav', 'n_unfav', 'n_Neu')
rownames(prod_data1) <- c('Age_18to30','Age_30to45','Age_45plus')
prod_data1 <- as.table(prod_data1)
prod_data1

prod_chisq <- chisq.test(prod_data1, p=x/sum(y))
print(prod_chisq, digits = 8)
qchisq(0.01, 4, lower.tail = F)

#####################################
#ch 16.3 practice Q2

#make matrix
Q2_data0 <- matrix(c(49,37,41,82,56,42,24,19),
                     ncol = 2, byrow = T)

colnames(Q2_data0) <- c('male', 'female')
rownames(Q2_data0) <- c('Below_25K','25Kto50K','50Kto75K', 'Above_75K')
Q2_data0 <- as.table(Q2_data0)
Q2_data0

#chi sq test
prod_chisq <- chisq.test(Q2_data0)
print(prod_chisq, digits = 8)
qchisq(0.025, 3, lower.tail = F)

########################################
#ch 16.3 practice Q3

#make matrix
Q3_data0 <- matrix(c(60,60.7,68,67.3,70,69.3,76,76.7),
                   ncol = 4, byrow = T)

colnames(Q3_data0) <- c('n_Diseased','En_Diseased', 'n_Not_Diseased','En_Not_Diseased')
rownames(Q3_data0) <- c('Vaccinated','Not_Vaccinated')
Q3_data0 <- as.table(Q3_data0)
Q3_data0

#chi sq test
prod_chisq <- chisq.test(Q3_data0)
print(prod_chisq, digits = 8)

#correct answer 0.029
qchisq(0.025, 1, lower.tail = F)

###########################################

#ch 16.3 certification Q1

#make matrix
Q1C_data0 <- matrix(c(23,15,23,55,34,22,20,13),
                   ncol = 2, byrow = T)

colnames(Q1C_data0) <- c('male', 'female')
rownames(Q1C_data0) <- c('Below_25K','25Kto50K','50Kto75K', 'Above_75K')
Q1C_data0 <- as.table(Q1C_data0)
Q1C_data0

#chi sq test
prod_chisq <- chisq.test(Q1C_data0)
print(prod_chisq, digits = 8)
qchisq(0.05, 3, lower.tail = F)
##########################################

#ch 16.3 certification Q2

#make matrix
Q2C_data0 <- matrix(c(49,15,18,
                       61,22,22,
                       64,23,24),
                     ncol = 3, byrow = T)

colnames(Q2C_data0) <- c('n_fav', 'n_unfav', 'n_Neu')
rownames(Q2C_data0) <- c('Age_18to30','Age_30to45','Age_45plus')
Q2C_data0 <- as.table(Q2C_data0)
Q2C_data0

#chi sq test
prod_chisq <- chisq.test(Q2C_data0)
print(prod_chisq, digits = 8)
qchisq(0.01, 4, lower.tail = F)

#######################################

#ch 16.3 practice Q3

#make matrix
Q3C_data0 <- matrix(c(49,46,43,46,56,59,62,59),
                   ncol = 4, byrow = T)

colnames(Q3C_data0) <- c('n_Diseased','En_Diseased', 'n_Not_Diseased','En_Not_Diseased')
rownames(Q3C_data0) <- c('Vaccinated','Not_Vaccinated')
Q3C_data0 <- as.table(Q3C_data0)
Q3C_data0

#chi sq test
prod_chisq <- chisq.test(Q3C_data0)
print(prod_chisq, digits = 8)

#correct answer:
qchisq(0.025, 1, lower.tail = F)
