library(tidyverse)
library(nlme)


#Ch 14. Q1 get prediction interval
Mother <- c(72,62,64,71,70,61,65,68,70,62,64,66,64)
Father <- c(71,65,67,76,74,74,71,63,68,63,75,74,75)
Daughter <- c(72,66,68,76,73,68,67,67,71,62,71,72,70)

m1 <- lm(Daughter~Mother+Father)
predict(m1, newdata=list(Mother=64, Father=74), interval="prediction", level=0.99)

#Ch 14. Q2 get prediction interval
Mother2 <- c(70,68,63,70,64,63,58,63,62,63,69,61,71)
Father2 <- c(70,67,74,70,69,65,67,74,75,64,74,68,65)
Daughter2 <- c(71,69,69,70,65,63,62,69,67,63,71,65,67)

m2 <- lm(Daughter2~Mother2+Father2)
predict(m2, newdata=list(Mother2=64, Father2=74), interval="prediction", level=0.99)

#Ch 14. Q3 get confidence interval

Mother3 <- c(62,60,71,67,69,68,72,60,67,66,62,70,68)
Father3 <- c(66,74,65,77,77,73,70,74,68,77,69,71,64)
Daughter3 <- c(63,65,67,72,74,71,70,66,70,72,67,70,66)

m3 <- lm(Daughter3~Mother3+Father3)
predict(m3, newdata=list(Mother3=64, Father3=74), interval="confidence", level=0.99)

#Ch 14. Q4 get 90% confidence interval

Mother4 <- c(68,59,60,67,67,64,62,64,59,71,62,63,67)
Father4 <- c(67,73,77,76,64,66,70,68,70,75,70,72,64)
Daughter4 <- c(69,63,68,71,64,64,65,64,64,71,63,66,64)

m4 <- lm(Daughter4~Mother4+Father4)
predict(m4, newdata=list(Mother4=64, Father4=74), interval="confidence", level=0.9)

#Ch 14. Q5 get 99% confidence interval

Mother5 <- c(70,62,72,69,66,69,67,65,69,59,72,58,72)
Father5 <- c(74,69,73,71,77,77,73,67,75,63,76,71,70)
Daughter5 <- c(69,64,70,70,69,70,68,64,70,58,71,61,69)

m5 <- lm(Daughter5~Mother5+Father5)
predict(m5, newdata=list(Mother5=64, Father5=74), interval="confidence", level=0.99)

#Ch 14. Q6 get 99% prediction interval

Mother6 <- c(72,62,64,71,70,61,65,68,70,62,64,66,64)
Father6 <- c(71,65,67,76,74,74,71,63,68,63,75,74,75)
Daughter6 <- c(72,66,68,76,73,68,67,67,71,62,71,72,70)

m6 <- lm(Daughter6~Mother6+Father6)
predict(m6, newdata=list(Mother6=64, Father6=74), interval="prediction", level=0.99)

#Ch 14. Q7 get 99% prediction interval

Mother7 <- c(70,62,72,69,66,69,67,65,69,59,72,58,72)
Father7 <- c(74,69,73,71,77,77,73,67,75,63,76,71,70)
Daughter7 <- c(69,64,70,70,69,70,68,64,70,58,71,61,69)

m7 <- lm(Daughter7~Mother7+Father7)
predict(m7, newdata=list(Mother7=64, Father7=74), interval="prediction", level=0.99)

#Ch 14. Q8 get 99% confidence interval

Mother8 <- c(72,62,64,71,70,61,65,68,70,62,64,66,64)
Father8 <- c(71,65,67,76,74,74,71,63,68,63,75,74,75)
Daughter8 <- c(72,66,68,76,73,68,67,67,71,62,71,72,70)

m8 <- lm(Daughter8~Mother8+Father8)
predict(m8, newdata=list(Mother8=64, Father8=74), interval="confidence", level=0.99)

#Ch 14. Q9 get 90% prediction interval

Mother9 <- c(65,63,67,60,65,68,67,71,58,58,58,58,61)
Father9 <- c(74,71,67,64,65,75,77,72,68,76,76,75,75)
Daughter9 <- c(68,68,68,59,65,69,72,73,62,65,65,67,67)

m9 <- lm(Daughter9~Mother9+Father9)
predict(m9, newdata=list(Mother9=64, Father9=74), interval="prediction", level=0.90)

#Ch 14. Q10 get 99% confidence interval

Mother10 <- c(68,58,61,62,64,61,67,64,62,68,60,68,69)
Father10 <- c(73,62,74,75,66,64,71,65,73,71,75,76,71)
Daughter10 <- c(69,59,66,66,62,61,69,63,66,69,66,70,68)

m10 <- lm(Daughter10~Mother10+Father10)
predict(m10, newdata=list(Mother10=64, Father10=74), interval="confidence", level=0.99)

#Ch 14. Q11 get 99% prediction interval

Mother11 <- c(68,59,60,67,67,64,62,64,59,71,62,63,67)
Father11 <- c(67,73,77,76,64,66,70,68,70,75,70,72,64)
Daughter11 <- c(69,63,68,71,64,64,65,64,64,71,63,66,64)

m11 <- lm(Daughter11~Mother11+Father11)
predict(m11, newdata=list(Mother11=64, Father11=74), interval="prediction", level=0.90)



