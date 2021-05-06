#population variance
n <- length(c(10,9,4,3,7,6,4,5,3,34))
n
my_pop_var_A <- ((n-1)/n)*var(c(10,9,4,3,7,6,4,5,3,34))
my_pop_var_A

my_pop_sd_a <- sqrt(my_pop_var_A)
my_pop_sd_a

my_range_a <- max(c(10,9,4,3,7,6,4,5,3,34)) - min(c(10,9,4,3,7,6,4,5,3,34))
my_range_a
#sample variance
var(c(14,-12,-10,14,-10,14,-10))
sd(c(14,-12,-10,14,-10,14,-10))
range(c(14,-12,-10,14,-10,14,-10))
my_range <- max(c(14,-12,-10,14,-10,14,-10)) - min(c(14,-12,-10,14,-10,14,-10))
my_range

my_freq <- c(60,109)
my_class <- (max(my_freq)-min(my_freq))/5
my_class

my_data <- data.frame(c(1364, 1458, 1214, 1270, 1226, 1320, 1213,
                        1168, 1211, 1333, 1363, 1323, 1177, 1316,
                        1147, 1423, 1179, 1211, 1483, 1156, 1280))

names(my_data) <- 'freq'

my_data1 <- my_data %>%
  filter(freq <= 1259)
  #filter(freq >= 1380 & freq <= 1439)
my_data1

#6+3+3
#6+9+3 = 18

age <- c(47,	46,	54,	48,	54,
         65,	55,	45,	70,	64,
         66,	52,	44,	42,	70)

weight <- sort(c(5.4,	6.9,	7.8,	6.2,	5.6,
                 6.0,	8.8,	5.5,	6.5,	7.9,
                 6.2,	8.8,	6.5,	7.9,	7.4,
                 7.0,	7.6,	6.4,	8.4,	8.2))
weight

#choose the highest z score
z1 <- (76.4-68.6)/13
z1
z2 <- (273.3-260)/19
z2
z3 <- (8.04-7.2)/0.4
z3

z4 <- (3.05-3.06)/0.01
z5 <- (5.04-5.06)/0.03


#my range
my_vector <- c(48,44,32,30,50,28,18,49,21,44,34,42,39,13,18)
sort(my_vector)
length(my_vector)

#use the ranked position to ID the quartile
length(my_vector)*(25/100) #for 1st quartile
length(my_vector)*(75/100) #for 3rd quartile

my_vector2 <- c(83,47,92,94,84,46,51,60,77,65)
sort(my_vector2)
length(my_vector2)
length(my_vector2)*0.25
length(my_vector2)*0.75


##get outliers
#step 1: find Q1 and Q3
outlier <- sort(c(41,55,29,1,30,23,9,37,6,23,18,54,6,46))
pos_Q1 <- length(outlier)*0.25
#9
pos_Q3 <- length(outlier)*0.75
#41

#step 2: get IQR (if decimals, round up always)
my_IQR <- 41 - 9

#step 3: get lower end point: =Q1-(1.5*IQR)
lower_endpt <- 9 - (1.5*my_IQR)

#step 4: get upper end point: =Q3+(1.5*IQR)
higher_endpt <- 41 + (1.5*my_IQR)

#step 5: check which values fall outside this range
print(c(lower_endpt, higher_endpt))


my_IQR2 <- sort(c(52,6,7,22,47,10,45,50,45,52,38,4,16,54,32))
my_IQR2_pos_Q1 <- length(my_IQR2)*.25
my_IQR2_pos_Q3 <- length(my_IQR2)*.75

weight2 <-length(sort(c(8.0,	7.2,	7.8,	8.1,	6.8,
  6.3,	8.7,	7.4,	7.8,	7.9,
  7.2,	7.8,	8.4,	6.2,	9.3)))

#get value, this is the position, 
#round up any decimal
#and find this position in your sorted array

weight2*(48/100)

v1 <- sort(c(70,49,48,87,67,59,74,53,59,80))
summary(v1)

length(v1)*0.25
length(v1)*0.75
summary(c(52,6,7,22,47,10,45,50,45,52,38,4,16,54,32))
sort(c(52,6,7,22,47,10,45,50,45,52,38,4,16,54,32))
length(sort(c(52,6,7,22,47,10,45,50,45,52,38,4,16,54,32)))
15*(.25)
15*(.75)
