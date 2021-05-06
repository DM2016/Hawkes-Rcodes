#Ch11.6 Practical or Statistical Significance
x_hat <- 43
u <- 40
std1 <- 8
n <- 120
df <- n-1

t <- (x_hat - u)/(std1/sqrt(n))
t

##Critical values for t
#left tailed
alpha <- 0.025
df <- n-1
qt(alpha,df)

#right tailed
alpha <- 0.025
df <- n-1
qt(1-alpha,df)

#two tailed
alpha <- 0.05
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