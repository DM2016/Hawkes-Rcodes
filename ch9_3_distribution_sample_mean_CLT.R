#Chr9 Distribution of the sample mean and Central Limit Theorem
#sample english(x,p,s^2)
#population greek(u,p,s^2)

valve <- c('A', 'B', 'C',
           'D', 'E', 'F')

diameter <- c(0.124, 0.136, 0.201,
              0.144, 0.138, 0.147)

valve_measurements <- data.frame(valve, diameter)
valve_measurements

mean(valve_measurements$diameter)
sd(valve_measurements$diameter)
##########################
library(greekLetters)
#pop of variance infinite size
#σ²meanx <- σ²/n

#pop of std infinite size
#standard error
#n=sample size
#σmeanx <- σpop/sqrt(n)

#var for sample mean
#N= pop size, #n=sample size
#σ²meanx = ((N-n)/(N-1))*(σ²/n)

#std for sample mean
#σmeanx = sqrt(((N-n)/(N-1))*(σ²/n))

#z score
#z <- (x-μ)/σmeanx
#z <- (x-μ)/(σ/sqrt(n))
#ch9 

####
#two tailed
population_mean <- 140
pop_std <- 14
pop_n <- 76
x1 <- -4.5
x2 <- 4.5

z_pop1 <- (x1)/(pop_std/sqrt(pop_n))
z_pop1

z_pop2 <- (x2)/(pop_std/sqrt(pop_n))
z_pop2

############
#one tailed negative z score table
#if question asks if greater, leave number alone
#if question asks if less, subtract from 1
sample_n <- 76
sample_mean <- 451
sample_std <- 12
x <- 449.8

z_sample <- (x-sample_mean)/(sample_std/sqrt(sample_n))
z_sample 

