#11.4a Testing a hypothesis about a pop proportion

p_hat <- 0.57   #sample proportion
p0    <- 0.60  #pop proportion
n     <- 1100  #sample size
  
std_prop <- sqrt((p0*(1-p0))/n)
std_prop

z <- (p_hat-p0)/std_prop
z

#2 tailed z score
alpha <- 0.01
z_adiv2 <- qnorm(1-alpha/2)
z_adiv2
