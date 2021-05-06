#Ch11.5 test hypo about  pop std or var

n <- 29                 #sample size
std_pop <- sqrt(0.19)   #standard dev of pop
s <-       0.3901 #sample standard dev

chi_sq <- ((n-1)*(s^2))/(std_pop^2)
chi_sq

#get chisq critical val
#If the observed chi-square test statistic is greater
#than the critical value, the null hypothesis can be rejected.
#two tailed chi
alpha <- 0.1
df <- n-1
qchisq(alpha/2, df, lower.tail = F)
qchisq(1-alpha/2, df, lower.tail = F)

#right tailed
alpha <- 0.025
df <- n-1
qchisq(alpha, df, lower.tail = F)

#left tailed
alpha <- 0.05
df <- n-1
qchisq(alpha, df, lower.tail = T)


#chi sq p values
#lower/left tail
df <- n-1
pchisq(chi_sq,df)

#upper right tail
df <- n-1
1-pchisq(chi_sq,df)
