#Ch 5.1 scatter corr

y <- c(55,55.5,57,57.5,58.5)
x <- c(4,3.5,3,2.5,1.5)

cor(x,y)

#Ch 5.3
library(Metrics)

College_GPA <- c(2.00,3.45,3.38,3.59,2.90,3.45)
HS_GPA <- c(3.49,3.77,4.64,2.23,3.45,3.75)
GPA <- data.frame(College_GPA, HS_GPA)

GPA_regression <- lm(College_GPA~HS_GPA)
GPA_SSE <- sum(residuals(GPA_regression)^2)
GPA_SSE

#MSE <- SSE/(n-2), n = 6
GPA_MSE <- GPA_SSE/(length(College_GPA)-2)
GPA_MSE

#get standard error
#square root MSE
sqrt(GPA_MSE)

#get slope
price <- c(31,40,41,46,50)
bids <- c(1,2,3,4,7)
price_bids <- lm(bids~price)
summary(price_bids)

0.29630*31+(-8.92593)

#coefficient of determination
#Multiple R-squared:  0.778

Age <- c(34,38,43,48,65)
Bone_Density <- c(349,347,338,324,323)
Bone_Density_model <- lm(Bone_Density~Age)
summary(Bone_Density_model)

(-0.8957*65)+377.0454


######
#coefficient of determination

ticket <- c(1,2,4,5,7)
gpa <- c(4,3.5,3,2.5,1)
hours_grades <- lm(gpa~ticket)
summary(hours_grades)

#Unemployment rates

year <- c(1,2,3,4,5,6,7,8,9,10)
UR <- c(3.3,4.8,11.2,6.5,7.8,
        6.7,7.4,8.3,8.9,7.6)

my_UR <- lm(UR~year)
summary(my_UR)

