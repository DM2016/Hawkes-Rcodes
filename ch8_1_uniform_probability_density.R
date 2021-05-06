#ch 8.1: uniform probability density function
#a = min range
#b =max range
a <- 50
b <- 66

uniform <- 1/(b-a) #for a <= x <=b, 0 otherwise

uniform_mean <- (a+b)/2
uniform_mean
uniform_std <- (b-a)/sqrt(12)
uniform_std
