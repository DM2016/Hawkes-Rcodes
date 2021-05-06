#ch10 point estimate

my_PE <- c(2.42,2.32,2.30,2.42,2.36,2.30)
n <- length(my_PE)
my_PE_mean <- round(mean(my_PE),3)
my_PE_mean
my_PE_var <- var(my_PE)
my_PE_var
my_PE_var <- var(my_PE)
my_PE_sd <- sd(my_PE)
my_PE_sd


#confidence interval
n <- 17
my_var <- var(c(12.7,12.9,12.1,11.7,12.0,12.5,12.6,12.3))
my_sd <- sd(c(12.7,12.9,12.1,11.7,12.0,12.5,12.6,12.3))
my_ci <- 0.98
my_deci <- 3
my_alpha <- 1-my_ci
my_alpha_tt <- my_alpha/2
my_compl_alpha_tt <- 1-my_alpha_tt

df <- n-1
my_chisq_max <- qchisq(my_alpha_tt, df=df) 
my_chisq_max

my_chisq_min <- qchisq(my_compl_alpha_tt, df=df) 
my_chisq_min

my_max_var <- (df*my_var)/my_chisq_max
my_min_var <- (df*my_var)/my_chisq_min

my_max_sd <- sqrt((df*my_var)/my_chisq_max)
my_min_sd <- sqrt((df*my_var)/my_chisq_min)

#variance
print(round(c(my_min_var, my_max_var),my_deci))

#standard dev
print(round(c(my_min_sd, my_max_sd),my_deci))

