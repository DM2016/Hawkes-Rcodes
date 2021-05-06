#Ch11.3 Cint hyp testing
#t stat for n <=30, pop std unknown, normal distribution
x_hat <- 1042.57
u <- 1130
std1 <- 229.69
n <- 100
df <- n-1

t <- (x_hat - u)/(std1/sqrt(n))
t

##Critical values for t
#left tailed
alpha <- 0.01
df <- 25
qt(alpha,df)

#right tailed
alpha <- 0.025
df <- 14
qt(1-alpha,df)

#two tailed
alpha <- 0.1
df <- n-1
t_adiv2 <- qt(1-alpha/2,df)
t_adiv2
##############################
#ci for t score
ci_lower <- x_hat - t_adiv2*(std1/(sqrt(n)))
ci_lower

ci_upper <- x_hat + t_adiv2*(std1/(sqrt(n)))
ci_upper

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


#####################################
#Ch11.3 Cint hyp testing
#z stat for n <=30, pop std unknown, normal distribution
x_hat <-  1042.57
u <- 1130
std1 <- 229.69
n <- 100
df <- n-1

z <- (x_hat - u)/(std1/sqrt(n))
z

###critical values for Z
#lower (left tailed)
alpha <- 0.05
qnorm(alpha)

#upper (right tailed)
alpha <- 0.01
qnorm(1-alpha)

#2 tailed z score
alpha <- 0.1
z_adiv2 <- qnorm(1-alpha/2)
z_adiv2

#ci for z score
ci_lower <- x_hat - z_adiv2*(std1/(sqrt(n)))
ci_lower

ci_upper <- x_hat + z_adiv2*(std1/(sqrt(n)))
ci_upper

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
