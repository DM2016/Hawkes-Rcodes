#Ch9,4 Distribution of the sample proportion
#n sample size, x number of interest, 

#E(p_hat) = p
#p_hat <- x/n

#std_p_hat <- sqrt((p_hat*(1-p_hat))/n) 

#get z score
#population proportion (p) is the stand in for z
#(p-Error_of_estimation)/std_p_hat < p < (p+Error_of_estimation)/std_p_hat
#for left hand results, subtract z score from 1
#for right hand results, leave z score alone
#right from left for final answer (L-R)

#for two way
p_hat <- 0.27
n <- 423

std_p_hat <- sqrt((p_hat*(1-p_hat))/n)
std_p_hat

Error_of_estimation <- 0.06
my_prop_max <- round((Error_of_estimation)/std_p_hat,2)
my_prop_max
my_prop_min <- round((-1 * Error_of_estimation)/std_p_hat,2)
my_prop_min

# set up probabilities
z_prob <- 2*round(pnorm(-abs(my_prop_max)), 4)
z_prob

comp_z_prob <- 1-z_prob
comp_z_prob

#if question is less than, leave z_final_prob alone
#if question is greater than, do final <-1- z_final_prob 

####
#for one way & neg z table
#if greater than, leave z-prob alone
#if less than, subtract z-prob from 1

p_hat <- 0.09
n <- 580

std_p_hat <- sqrt((p_hat*(1-p_hat))/n)
std_p_hat

Error_of_estimation <- 0.11
my_prop <- round((p_hat-Error_of_estimation)/std_p_hat,2)
my_prop

#get probabilities
z_prob <- round(pnorm(-abs(my_prop)), 4)
z_prob

comp_z_prob <- 1-z_prob
comp_z_prob


