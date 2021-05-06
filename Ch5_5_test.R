#Chr5. Fit line model

my_scores <- read.csv('D:/Docker/Hawkes_Rcodes/Ch5_info1_data.csv', header = T)
my_scores 

options(digits = 6)
my_scores_model <- lm(my_scores$Second.Test.Grade~my_scores$First.Test.Grade)
my_scores_model
############################

high_school_GPA <- c(3.34,2.14,2.09,2.93,2.26,3.66)
college_GPA <- c(2.78,3.70,2.27,3.47,3.14,3.95)

my_scores <- data.frame(college_GPA, high_school_GPA)
my_scores

my_gpa <- lm(college_GPA~high_school_GPA)
my_gpa

summary(my_gpa)
anova(my_gpa) #sum of squared errors answer is in residuals

########################

options(digits=6)
y <- 15.99+(0.12*1219)
y

##########################
#Error
#predicted - observed
my_error <- 162.27-183.8
my_error

my_squared_error <- my_error^2
my_squared_error
###################################

(200-45.61)/0.52

################

my_num <- c(21,	34,	37,	45,	46,	43,
  25,	49,	34,	46,	38,	46,
  55,	26,	28,	38,	47,	35)

my_num2 <- c(15,16,	13,	1,	3,	5,	10,	7,
             7,	8,	6,	2,	11,	2,	3,	13)

table(sort(my_num2))
