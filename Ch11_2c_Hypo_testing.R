#Ch11.2c Hypo testing
x_hat <- 6.4   #sample average
u <- 6.2       #pop/true mean
std1 <- 0.9     #standard dev
n <- 170        #sample number

z <- (x_hat - u)/(std1/sqrt(n))
z

###critical values for Z
#lower (left tailed)
alpha <- 0.05
qnorm(alpha)

#upper (right tailed)
alpha <- 0.01
qnorm(1-alpha)

#2 tailed
alpha <- 0.1
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

#################################
#t stat for n <=30, pop std unknown, normal distribution
x_hat <- 6.0
u <- 5.5
std1 <- 0.9
n <- 13
df <- n-1

t <- (x_hat - u)/(std1/sqrt(n))
t

##Critical values for t
#left tailed
alpha <- 0.05
df <- 25
qt(alpha,df)

#right tailed
alpha <- 0.025
df <- 14
qt(1-alpha,df)

#two tailed
alpha <- 0.02
df <- 12
qt(1-alpha/2,df)

#t stat p vals

#left tail
df <- n - 1
pt(t,df)

#right tailed
df <- n -1
(1-pt(t,df))

#two tailed
df <- n -1
2*(1-pt(t,df))

