#ch12.1a Inference about two means:
#Independent Samples with sigma known
# if both n1 and n2 > 30
#normal distribution

#u_mean_diff <- u1 - u2

#std_mean_diff <- sqrt(((std1^2)/n1) + ((std2^2)/n2))

#t.test(dependent~independent, dataset, conf.level)

#if u1 > u2, then alt hyp is right tailed
#if u1 < u2, the alt hyp is left tailed

##################################
# z score when both n1 and n2 > 30

#x_hat1: sample mean 1
#x_hat2: sample mean 2
#u1: pop mean 1
#u2: pop mean 2
#std1: pop std 1
#n1: sample size 1
#std2: pop std 2
#n2: sample size 2

#z <- ((x_hat1-x_hat2)-(u1-u2))/sqrt((std1^2/n1)+(std2^2/n2))
#z

#ci: confidence interval
#alpha <- 1-ci

#Left tail
#z_crit <- qnorm(alpha, lower.tail=T)

#Right tail
#z_crit <- qnorm(alpha, lower.tail=F)

#2 tail
#z_crit <- qnorm(alpha/2, lower.tail=F)

#t_crit <- qt(1-alpha/2, df, lower.tail=F)
#pt(-abs(t), df = n - 1) one tail
#2*pt(-abs(t), df = n - 1) two tail

#ci_lower <- (x_hat1-x_hat2)-z_crit*sqrt((std1^2/n1)+(std2^2/n2))
#ci_lower
#ci_upper <- (x_hat1-x_hat2)+z_crit*sqrt((std1^2/n1)+(std2^2/n2))
#ci_upper

x_hat1 <- 78
x_hat2 <- 81

n1 <- 36
n2 <- 36

std1 <- 7
std2 <- 9.6

u1 <-0
u2 <-0

my_z <- round(((x_hat1-x_hat2)-(u1-u2))/sqrt((std1^2/n1)+(std2^2/n2)),2)
my_z

ci <- 0.95
alpha <- 1-ci
pt(-abs(my_z), df = (n1+n2)-2)

#two tailed
z_crit <- qnorm(alpha/2, lower.tail=F)
z_crit

#left tailed
left_z_crit <- qnorm(alpha, lower.tail=T)
left_z_crit

ci_lower <- (x_hat1-x_hat2)-z_crit*sqrt(((std1^2)/n1)+((std2^2)/n2))
ci_lower

ci_upper <- (x_hat1-x_hat2)+z_crit*sqrt(((std1^2)/n1)+((std2^2)/n2))
ci_upper



