#Chr12.1b

n1 <- 13
u1 <- 1070
std1 <- 42

n2 <- 8
u2 <- 1010
std2 <- 43

Sp <- sqrt(((n1-1)*(std1^2)+(n2-1)*(std2^2))/(n1+n2-2))
Sp

t_stat <- (u1-u2)/(Sp*sqrt((1/n1)+(1/n2)))
round(t_stat, 3)

#for equal variances
df <- n1 + n2 - 2
df

#for unequal variances
#use which ever df for n1 or n2 is smaller

#probability
pt(t_stat, df)

#critical t val
alpha <- 0.1
ci <- 1 - alpha

#for the left
left_t_crit <- -abs(qt(alpha, df))
round(left_t_crit, 3)
#reject if test less than critical value

#for the right
right_t_crit <- abs(qt(alpha, df))
round(right_t_crit, 3)

#reject if test more than critical value

#for two tailed
two_tailed_t_crit <- abs(qt(alpha/2, 7))
round(two_tailed_t_crit, 3)

#standard error of sampling distribution
SE <- sqrt((std1^2/n1)+(std2^2/n2))
round(SE,2)

#for ci
ci_lower <- (u1-u2) - two_tailed_t_crit*round(SE, 2)
ci_lower

ci_upper <- (u1-u2) + two_tailed_t_crit*round(SE, 2)
ci_upper

