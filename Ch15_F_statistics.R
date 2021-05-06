#Ch15 New

Airline_A <- c(70,80,75,84)
Airline_B <- c(72,72,78,90)
Airline_C <- c(74,82,90,88)
Airline_D <- c(75,89,85,82)


airlines <- data.frame(Airline_A, Airline_B, 
                       Airline_C, Airline_D)

#part1
MST <- 41.0833 #mean squared treatment (columns)
MSE <- 24.0278 #mean squared error
my_F_stat <- MST/MSE
my_F_stat

#part2
#get critical F value
p <- 0.1
k <- 4
b <- 4
df1 <-  (k-1)      #numerator degrees of freedom
df2 <-  (k-1)*(b-1)#denominator degrees of freedom
#lower tail: T for left tail, F for Right tail

#Perform F test crit val
#Reject if test stat greater than or equal to critical val
my_F_crit_val <-  qf(p, df1, df2, lower.tail = F)
my_F_crit_val

#Test my test stat vs the critical val
# my_F_crit_val   my_F_stat
# 2.81286         1.70982

#my F stat is less than critical val, Fail to reject H0
##########################################################

car_MST <- 53.1333
car_MSE <- 14.7583

my_F_stat <- car_MST/car_MSE
my_F_stat

#get critical F value
p <- 0.1
k <- 4
b <- 5
df1 <-  (k-1)      #numerator degrees of freedom
df2 <-  (k-1)*(b-1)#denominator degrees of freedom
#lower tail: T for left tail, F for Right tail

#Perform F test crit val
#Reject if test stat greater than or equal to critical val
my_F_crit_val <-  qf(p, df1, df2, lower.tail = F)
my_F_crit_val
###############################
52.9167/37.6389

#get critical F value
p <- 0.1
k <- 3
b <- 4
df1 <-  (k-1)      #numerator degrees of freedom
df2 <-  (k-1)*(b-1)#denominator degrees of freedom
#lower tail: T for left tail, F for Right tail

#Perform F test crit val
#Reject if test stat greater than or equal to critical val
my_F_crit_val <-  qf(p, df1, df2, lower.tail = F)
my_F_crit_val
###########################
