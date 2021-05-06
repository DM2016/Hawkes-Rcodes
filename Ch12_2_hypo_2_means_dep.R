#Ch12.2 hypo 2 means dep

library(tidyverse)
first_sat <- c(530,360,580,450,530,560,380)
second_sat <- c(550,410,610,510,550,600,430)

n <- length(first_sat)
n

sat_scores0 <- data.frame(first_sat, second_sat)

sat_scores1 <- sat_scores0 %>%
  mutate(
    diff = first_sat - second_sat,
    diff_sq = diff^2
  )

sat_scores1

my_diff_sq_sum_L <- sum(sat_scores1$diff_sq)
my_diff_sum_sq_R <- (sum(sat_scores1$diff))^2

std_pd <- sqrt(((n*my_diff_sq_sum_L) - my_diff_sum_sq_R)/(n*(n-1)))
round(std_pd, 1)

#hyp test mean of paired differences
x_hatd <- sum(sat_scores1$diff)/n
x_hatd

t <- x_hatd/((round(std_pd, 1))/sqrt(n))
round(t, 3)


t.test(first_sat,second_sat, alternative = "less", conf.level = 0.9)

#for the left
alpha <- 0.05
df <- n-1
left_t_crit <- -abs(qt(alpha, df))
round(left_t_crit, 3)
#########################################################
no_additive <- c(17.7,11.1,27.2,18.7,20.2,26.2,19.8,26,9.6,23.5)
yes_additive <- c(20,14.2,28.3,21.1,21.7,27.5,20.5,28.2,12.8,24.9)

n <- length(no_additive)
n

#d gas mileage with additive - gas milage w/o additive
car_test0 <- data.frame(no_additive, yes_additive)

car_test1 <- car_test0 %>%
  mutate(
    diff = yes_additive - no_additive,
    diff_sq = diff^2
  )

car_test1

my_diff_sq_sum_L <- sum(car_test1$diff_sq)
my_diff_sum_sq_R <- (sum(car_test1$diff))^2

std_pd <- sqrt(((n*my_diff_sq_sum_L) - my_diff_sum_sq_R)/(n*(n-1)))
round(std_pd, 2)

#hyp test mean of paired differences
x_hatd <- sum(car_test1$diff)/n
x_hatd

t <- x_hatd/((round(std_pd, 1))/sqrt(n))
round(t, 3)


#for the right
alpha <- 0.01
df <- n -1
right_t_crit <- abs(qt(alpha, df))
round(right_t_crit, 3)

#reject if test more than critical value
################################################################

old_score <- c(77,85,95,80,80,78,88,95)
new_score <- c(72,90,92,76,84,75,82,93)

n <- length(old_score)
n

golf_scores0 <- data.frame(old_score, new_score)

#d=(golf score after using the newly designed golf clubs) minus 
# (golf score before using the newly designed golf clubs)

golf_scores1 <- golf_scores0 %>%
  mutate(
    diff = new_score - old_score,
    diff_sq = diff^2
  )

golf_scores1

my_diff_sq_sum_L <- sum(golf_scores1$diff_sq)
my_diff_sum_sq_R <- (sum(golf_scores1$diff))^2

std_pd <- sqrt(((n*my_diff_sq_sum_L) - my_diff_sum_sq_R)/(n*(n-1)))
round(std_pd, 1)

#hyp test mean of paired differences
x_hatd <- sum(golf_scores1$diff)/n
x_hatd

t <- x_hatd/((round(std_pd, 1))/sqrt(n))
round(t, 3)

#for the left
alpha <- 0.01
df <- n-1
left_t_crit <- -abs(qt(alpha, df))
round(left_t_crit, 3)

p_val <- pt(-abs(round(t, 3)), df)
round(p_val, 4)

##########################################

before_temp <- c(100.3, 99.8, 99.2, 100.5, 98.9, 100.6, 100)
after_temp <- c(99.9, 99.6, 99.3, 100.3, 98.7, 100.3, 99.8)

n <- length(before_temp)
n

body_temps0 <- data.frame(before_temp, after_temp)

#d=body temp after drug minus 
# body temp before drug

body_temps1 <- body_temps0 %>%
  mutate(
    diff = after_temp - before_temp,
    diff_sq = diff^2
  )

body_temps1

my_diff_sq_sum_L <- sum(body_temps1$diff_sq)
my_diff_sum_sq_R <- (sum(body_temps1$diff))^2

std_pd <- sqrt(((n*my_diff_sq_sum_L) - my_diff_sum_sq_R)/(n*(n-1)))
round(std_pd, 2)

#hyp test mean of paired differences
x_hatd <- sum(body_temps1$diff)/n
x_hatd

t <- round(x_hatd, 3)/((round(std_pd, 2))/sqrt(n))
round(t, 3)

#for the two tailed
alpha <- 0.2
df <- n-1
two_tailed_t_crit <- -abs(qt(alpha/2, df))
round(two_tailed_t_crit, 3)

p_val <- 2*pt(-abs(round(t, 3)), df)
two_t_p_val <- round(p_val, 4)
two_t_p_val

##############################
pop_1 <- c(44,31,35,24,47,20,41)
pop_2 <- c(30,25,42,36,49,34,43)

#d=(Population 1 entry)−(Population 2 entry)
my_pop0 <- data.frame(pop_1, pop_2) %>%
  mutate(
    pop_diff = pop_1 - pop_2,
  )

mean_pop <- mean(my_pop0$pop_diff)
mean_pop
std_pop <- sd(my_pop0$pop_diff)
std_pop

#two tailed
alpha <- 0.10
n <- length(pop_1)
df <- n - 1
two_tailed_t_crit <- abs(qt(alpha/2, df))
two_tailed_t_crit
round(two_tailed_t_crit, 3)

ci_lower <- mean_pop - round(two_tailed_t_crit, 3)*(std_pop/sqrt(n))
round(ci_lower, 1)
ci_upper <- mean_pop + round(two_tailed_t_crit, 3)*(std_pop/sqrt(n))
round(ci_upper, 1)
