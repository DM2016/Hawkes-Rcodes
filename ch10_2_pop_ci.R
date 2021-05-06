#ch10.2 interval estimation of the pop mean
#for normally distributed pop

my_std <- my_sample_std/sqrt(n)

x_hat <-
μ <-
σ_x <- 


z <- (x_hat - μ)/σ_x

#for n > 30
#ci for pop mean is
#min_ci_z <- x_hat-z_adiv2*(σ/sqrt(n))
#max_ci_z <- x_hat+z_adiv2*(σ/sqrt(n))

x_hat <- 4.1
my_sigma <- 1.4
n <- 576
ci <- 0.85
alpha <- 1 - ci
z_adiv2 <- qnorm(1-alpha/2)
z_adiv2 

min_ci_z <- x_hat-z_adiv2*(my_sigma/sqrt(n))
min_ci_z

max_ci_z <- x_hat+z_adiv2*(my_sigma/sqrt(n))
max_ci_z 

#for n < 30
#sample mean with σ unknown
# s is sample
# t = (x_hat-μ)/(s/sqrt(n))

#df = n - 1
#min_ci_t <- x_hat-t_adiv2*(s/sqrt(n))
n <- 27
df <- n-1
my_deci <- 3

my_mean <- 58.03
my_mean

my_sd <- 26.34
my_sd

#two sided t val, divide alpha in 2
ci <- 0.90
alpha <- 1-ci
my_crit_t_val <- round(abs(qt(alpha/2, df)), my_deci)
my_crit_t_val

min_ci_t <- my_mean-my_crit_t_val*(my_sd/sqrt(n))
min_ci_t

#max_ci_t <- x_hat+t_adiv2*(s/sqrt(n))
max_ci_t <- my_mean+my_crit_t_val*(my_sd/sqrt(n))
max_ci_t

#get sample size to estimate a population
#E = margin or error
#n = ((z_adiv2*σ)/E)^2

#get sample size
##n = ((z_adiv2*s)/E)^2
s <- 6.4
E <- 0.46
ci <- 0.95
alpha <- 1 - ci
z_adiv2 <- qnorm(1-alpha/2)
z_adiv2

n = ((z_adiv2*s)/E)^2
n
