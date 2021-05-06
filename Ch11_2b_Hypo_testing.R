#Ch11.2b Hypo testing
x_hat <- 37.8   #sample average
u <- 37.4   #pop/true mean
std1 <- 2.3    #standard dev
n <- 280      #sample number

z <- (x_hat - u)/(std1/sqrt(n))
z
#################################
#t stat for n <=30, pop std unknown, normal distribution
x_hat <- 419
u <- 425
std1 <- 13
n <- 10
df <- n-1

t <- (x_hat - u)/(std1/sqrt(n))
t

##Critical values for t
#left tailed
alpha <- 0.1
df <- 9
qt(alpha,df)

#right tailed
alpha <- 0.025
df <- 14
qt(1-alpha,df)

#two tailed
alpha <- 0.05
df <- 25
qt(1-alpha/2,df)
  
###critical values for Z
#lower (left tailed)
alpha <- 0.05
qnorm(alpha)

#upper (right tailed)
alpha <- 0.05
qnorm(1-alpha)

#2 tailed
alpha <- 0.05
qnorm(1-alpha/2)

###p values
#left tailed
p_val_left <- pnorm(z)
p_val_left

#right tailed
p_val_right <- 1-pnorm(z)
p_val_right

#two tailed
p_val_tt <- 2*(1-pnorm(z))
p_val_tt
