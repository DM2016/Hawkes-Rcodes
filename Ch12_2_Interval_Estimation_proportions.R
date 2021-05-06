#Ch12.3 Construct Interval Estimation for proportions

#proportion 1
x1 <- 9
n1 <- 95
p_hat1 <- x1/n1 
p_hat1
p_hat1_term <- ((p_hat1*(1-p_hat1))/n1)
p_hat1_term

#proportion 2
x2 <- 5
n2 <- 80
p_hat2 <- x2/n2 
p_hat2
p_hat2_term <- ((p_hat2*(1-p_hat2))/n2)
p_hat2_term

#computed weight estimate sample proportion
p_hat3 <- (x1+x2)/(n1+n2)
p_hat3

#proportion differences
p_hat_diff <- p_hat1-p_hat2
p_hat_diff

#proportion z statistic
#pop1 <- 
#pop2 <-
#pop_diff <- pop1-pop2
#pop_diff #use in below equation if known
z <- p_hat_diff/sqrt(p_hat3*(1-p_hat3)*((1/n1)+(1/n2)))
z

#z_adiv2 is the critical value
#use alpha and df to get this
#df <- n - 1
alpha <- 0.05

#z value critical val two-tailed
z_adiv2_tt <- qnorm(1-(alpha/2))
z_adiv2_tt

z_p_val_tt <- 2*(1-pnorm(z_adiv2_tt))
z_p_val_tt

#z value critical val right tailed
#p1 - p2 > 0
#reject if test stat >= critical val
alpha <- 0.01
z_adiv2_right <- qnorm(1-alpha)
z_adiv2_right

#z value for left tailed
#p1 - p2 <= 0
#reject if test stat < critical val
alpha <- 0.05
z_adiv2_left <- qnorm(alpha)
z_adiv2_left

#confidence intervals
ci_lower <- p_hat_diff - z_adiv2_tt*sqrt(p_hat1_term + p_hat2_term)
ci_lower

ci_upper <- p_hat_diff + z_adiv2_tt*sqrt(p_hat1_term + p_hat2_term)
ci_upper
##################################
alpha <- 0.05

#z value critical val two-tailed
z_adiv2_tt <- qnorm(1-(alpha/2))
z_adiv2_tt

#standard error, proportions
#term1
n1 <- 760
p_hat1 <- 0.43
p_hat1_term <- round(((p_hat1*(1-p_hat1))/n1),4)
p_hat1_term

#term2
n2 <- 660
p_hat2 <- 0.47
p_hat2_term <- round(((p_hat2*(1-p_hat2))/n2),4)
p_hat2_term

std_er_prop <- sqrt(p_hat1_term + p_hat2_term)
std_er_prop

#confidence intervals
p_hat_diff <- p_hat1-p_hat2
p_hat_diff

ci_lower <- p_hat_diff - z_adiv2_tt*sqrt(p_hat1_term + p_hat2_term)
ci_lower

ci_upper <- p_hat_diff + z_adiv2_tt*sqrt(p_hat1_term + p_hat2_term)
ci_upper

#################################
x1 <- 206
n1 <- 491
p_hat1 <- x1/n1
p_hat1
p_hat1_term <- ((p_hat1*(1-p_hat1))/n1)
p_hat1_term

x2 <- 311
n2 <- 569
p_hat2 <- x2/n2
p_hat2
p_hat2_term <- ((p_hat2*(1-p_hat2))/n2)
p_hat2_term

#z value critical val two-tailed
alpha <- 0.10
z_adiv2_tt <- qnorm(1-(alpha/2))
z_adiv2_tt

#get SE
std_er_prop <- sqrt(p_hat1_term + p_hat2_term)
std_er_prop


#get ci
p_hat_diff <- p_hat1-p_hat2
p_hat_diff

ci_lower <- p_hat_diff - z_adiv2_tt*sqrt(p_hat1_term + p_hat2_term)
ci_lower

ci_upper <- p_hat_diff + z_adiv2_tt*sqrt(p_hat1_term + p_hat2_term)
ci_upper




