#10.3 estimating population proportion

#x <- number in the sample that has the attribute
#n <- sample size
#p_hat <- proportion of sample (x/n)
#p <- proportion of pop
#std_p_hat <- sqrt((p_hat*(1-p_hat))/n)
#z <- (p_hat-p)/std_p_hat
#z_adiv2 <- qnorm(1-alpha/2)
#ci_min <- p_hat-z_adiv2*std_p_hat
#ci_max <- p_hat+z_adiv2*std_p_hat
#n <- (((z_adiv2)^2)*p_hat*(1-p_hat))/(E^2)

x <- 400
n <- 1267
p_hat <- round(x/n,3)
p_hat
ci <- 0.98
alpha <- 1-ci


std_p_hat <- round(sqrt((p_hat*(1-p_hat))/n),4)
std_p_hat

z_adiv2 <- round(qnorm(1-alpha/2),4)
z_adiv2
ci_min <- round(p_hat-z_adiv2*std_p_hat,3)
ci_min

ci_max <- round(p_hat+z_adiv2*std_p_hat,3)
ci_max
#########################
p_hat <- 0.48
E <- 0.03
ci <- 0.99
alpha <- 1-ci
z_adiv2 <- round(qnorm(1-alpha/2),4)
z_adiv2

n <- (((z_adiv2)^2)*p_hat*(1-p_hat))/(E^2)
n
