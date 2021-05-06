#CH14.5 regression coeff CI
library(tidyverse)
library(nlme)

Mother <- c(70,62,72,69,66,69,67,65,69,59,72,58,72)
Father <- c(74,69,73,71,77,77,73,67,75,63,76,71,70)
Daughter <- c(69,64,70,70,69,70,68,64,70,58,71,61,69)

m1 <- lm(Daughter~Mother+Father)
my_res <- predict(m1, newdata=list(Mother=64, Father=74), interval="prediction", level=0.99)
round(my_res, 2)
