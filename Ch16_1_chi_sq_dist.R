#ch16.1 chi square distribution
library(tidyverse)
library(qwraps2)
library(MASS)
library(janitor)

Lenders <- c('EB','AL','GW','GL','FS','AB','QK','CC')
APR <- c(3.519,3.643,4.049,4.229,4.209,4.029,4.439,4.130)

APR_sd <- sd(APR)
APR_var <- var(APR)
n <- length(Lenders)
n
pop_sd <- 0.1

#chisq.test
chi_sq_value <- ((n-1)*(APR_sd^2))/(pop_sd^2)
chi_sq_value

#chi sq value
qchisq(0.01, 7, lower.tail = F)

#get Chi-Sq critical val
#right tail
p <- 0.05
df <- 11
qchisq(p, df, lower.tail=FALSE)

#left tail
p <- 0.01
df <- 7
qchisq(p, df, lower.tail=TRUE)
###############################

options(digits=6)
weekly_sales <- c(17501,19897,28043,16569,22817,22871,18797,15206)
n <- length(weekly_sales)
pop_sd <- 5800

weekly_sales_sd <- sd(weekly_sales)
chi_sq_value <- ((n-1)*(weekly_sales_sd^2))/(pop_sd^2)
chi_sq_value

#chi sq value
qchisq(0.01, 7, lower.tail = F)
####################################

