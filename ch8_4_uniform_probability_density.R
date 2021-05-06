#ch 8.4 applicationa of normal distribution

x <- 59       #normal random variable
u <- 40         #mean
my_std <- 9     #standard deviation

  
z <- (x-u)/my_std
z

0.7486-0.0228
#alternative
z <- 
z <- -0.78
u <- 75.1
my_std <-9.1

x <- (z*my_std)+u
x
##?###################################
#ch 8.6 approximation to binomial dist
#check if np???10 and  n(1???p)???10

n <- 149
p <- 0.11
EX <- n*p
EX


my_std <- sqrt((n*p)*(1-p)) 
my_std

#lower
x_lower <- 14.5
x_upper <- 91.5

z_lower <- (x_lower-EX)/my_std
z_lower

z_upper <- (x_upper-EX)/my_std
z_upper
