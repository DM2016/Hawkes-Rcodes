#11.4b Testing a hypothesis about a pop proportion

p_hat <- 0.50   #sample proportion
p0    <- 0.55  #pop proportion
n     <- 210  #sample size

std_prop <- sqrt((p0*(1-p0))/n)
std_prop

z <- round((p_hat-p0)/std_prop,3)
z

#2 tailed z score
alpha <- 0.05
z_adiv2 <- qnorm(1-alpha/2)
z_adiv2
##################################

###p values
#When performing a hypothesis test using P-values:
#If the P-value is less than or equal to α, then reject the null hypothesis.
#If the P-value is greater than α, then we fail to reject the null hypothesis.

#left tailed
p_val_left <- pnorm(z)
p_val_left

#right tailed
p_val_right <- 1-pnorm(z)
p_val_right

#two tailed
p_val_tt <- 2*pnorm(z)
p_val_tt
