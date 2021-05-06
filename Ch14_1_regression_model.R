#Ch14.1 regression model

x1A <- 8
x1B <- 10
x2 <- 5
b0 <- 11681.31
b1 <- 3418.97
b2 <- 1194.78

estimated_salary1 <- b0 + b1*x1A + b2*x2
estimated_salary1

estimated_salary2 <- b0 + b1*x1B + b2*x2
estimated_salary2

estimated_differences <- estimated_salary2 - estimated_salary1
estimated_differences
