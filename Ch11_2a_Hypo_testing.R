#Ch11.2a Hypo testing
x_hat <- 37.8   #sample average
u <- 37.4   #pop/true mean
std1 <- 2.3    #standard dev
n <- 280      #sample number
  
z <- (x_hat - u)/(std1/sqrt(n))
z

#lower (left tailed)
alpha <- 0.05
qnorm(alpha)

#upper (right tailed)
alpha <- 0.02
qnorm(1-alpha)

#2 tailed
alpha <- 0.05
qnorm(1-alpha/2)
