library(FinCal)
#Chebyshev's theorem
my_mean <- 7.3
x <- 1-0.889 # capture 88.9% of data
x

#0.111111 
#this is 1/9
x2 <- 1/9

y = sqrt(x2)
y

#0.33333
#this is 1/3, take 3 and multiply by 
#standard deviation to get final answer

std <- 0.3

upper_range <- my_mean + 3*std
upper_range
lower_range <- my_mean - 3*std
lower_range

print(c(lower_range, upper_range))

my_sample1 <- c(9,10,10,7,4,8,5,8,8,34)
my_range <- max(my_sample1) - min(my_sample1)
my_std <- sd(my_sample1)
my_var <- my_std^2
print(c(my_range,my_std,my_var))

#the larger cv is the larger spread
my_cvA <- c(33300, 19000, 35200, 33500, 20200, 
            27800, 19900, 29900, 34500, 23000, 
            33200, 20600, 19200, 31700)

my_cvA1 <- mean(my_cvA)
my_cvA2 <- sd(my_cvA)
my_cvA3 <- coefficient.variation(my_cvA2,my_cvA1)

my_cvB <- c(4.70, 3.68, 3.98, 3.25, 3.28, 4.56, 4.01, 3.60, 3.44, 3.00, 4.43)

my_cvB1 <- mean(my_cvB)
my_cvB2 <- sd(my_cvB)
my_cvB3 <- coefficient.variation(my_cvB2,my_cvB1)

print(c(my_cvA3, my_cvB3))
#as a percentage
print(c(my_cvA3*100, my_cvB3*100))

sd(c(100.1,	99.6,	96.4,	98.8,	97.6,
     100.1,	99.6,	98.3,	99.8,	100.6,
     99.3,	96.6,	100.1,	97.6,	98.5,
     98.8,	99.9,	97.6,	96.7,	97.5))
