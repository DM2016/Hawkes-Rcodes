#chr13.2 sum of squares
n <- 5
comp_percent <- c(60,60,64,65,66)
intercept_percent <- c(4.6,2.5,2.5,2.2,1.2)

my_entries <- lm(intercept_percent~comp_percent)
my_entries
my_SSE <- sum(my_entries$residuals^2)
round(my_SSE, 3)

var_of_errors <- round(my_SSE, 3)/(n-2)
var_of_errors

var_of_slope_c1 <- sum(comp_percent^2)
var_of_slope_c2 <- (sum(comp_percent)^2)/n
var_of_slope <- var_of_errors/(var_of_slope_c1-var_of_slope_c2)
round(var_of_slope, 3)

round(confint(my_entries, level = 0.98),3)
round(confint(my_entries, level = 0.90),3)
#############################################

my_age <- c(30,50,40,55,30,28,60,25,30,45)
sick_days <- c(7,4,3,2,9,10,0,8,5,2)

my_sick_model <- lm(sick_days~my_age)
my_sick_model

summary(my_sick_model)
x <- 41
m <- -0.2369
b <- 14.310162
y <- b+m*x
y
se <- 1.682207

my_res <- predict(my_sick_model, newdata=list(my_age=41, sick_days=4.597262), interval="confidence", level=0.99)
round(my_res, 2)

#####################

training_hrs <- c(1,4,7,3,2,2,5,5,1,6)
defects <- c(5,1,0,3,5,4,1,2,8,2)

my_training_model <- lm(defects~training_hrs)
my_training_model

summary(my_training_model)

x <- 6
m <- -1.004950
b <- 6.717822
y <- b+m*x
y
se <- 1.229787

my_res <- predict(my_training_model, newdata=list(training_hrs=6, defects=0.688122), interval="prediction", level=0.90)
round(my_res, 2)
