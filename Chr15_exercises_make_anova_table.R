#make anova table for R, get F stat
library(tidyverse)

Area1 <- c(22,15,12,21,15,17,14,21)
Area2 <- c(16,8,22,17,11,9,11,10)
Area3 <- c(17,23,24,25,9,19,12,23)

Area <- data.frame(Area1, Area2, Area3)

#test the dataframe
head(Area)
summary(Area)

Stacked_Area <- stack(Area)
Stacked_Area

#fit regression model
Area.aov <- aov(values ~ ind, data = Stacked_Area)

#view output of model
summary(Area.aov)

#Chr 15, Q2
NYC <- c(322,234,295,296,288)
LA <- c(193,202,233,211,240)
ATL <- c(205,217,253,297,315)
HOU <- c(286,240,238,270,315)
PHX <- c(227,312,262,286,232)

#make dataframe
cities <- data.frame(NYC, LA, ATL, HOU, PHX)
stacked_cities <- stack(cities)

#fit regression model
cities.aov <- aov(values ~ ind, data = stacked_cities)
cities.aov
#test model
summary(cities.aov)

#chr 15.2
N <- 16
k <- 4
SSE <- 216.2500
SST <- 709.7500

my_DFT <- k - 1
my_DFE <- N - k

MST <- SST/my_DFT
MST

MSE <- SSE/my_DFE
MSE

my_F_dist <- MST/MSE
round(my_F_dist, 2)


Airline_A <- c(70,80,75,84)
Airline_B <- c(72,72,78,90)
Airline_C <- c(74,82,90,88)
Airline_D <- c(75,89,85,82)


airlines <- data.frame(Airline_A, Airline_B, 
                       Airline_C, Airline_D)

stacked_airlines <- stack(airlines)

airlines
airlines.aov <- aov(values ~ ind, data = stacked_airlines)
airlines.aov

summary(airlines.aov)



#Chr 15, Q3
#make dataframe
WK1 <- c(187, 191, 193, 195, 178)
WK2 <- c(174, 159, 173, 156, 151)
WK3 <- c(187, 188, 169, 182, 184)
WK4 <- c(162, 195, 164, 154, 195)

All_WK <- data.frame(WK1, WK2, WK3, WK4)

stacked_WKS <- stack(All_WK)

#fit regression model
All_WKS.aov <- aov(values ~ ind, data = stacked_WKS)

#test model
summary(All_WKS.aov)
###########################################

#ch15 Quiz qs

#Chr 15, Q1
NYC <- c(275, 313, 336, 233, 338)
LA <- c(340, 186, 220, 221, 170)
ATL <- c(332, 252, 350, 263, 256)
HOU <- c(266, 239, 309, 237, 245)
PHX <- c(318, 345, 293, 287, 318)

#make dataframe
cities <- data.frame(NYC, LA, ATL, HOU, PHX)
stacked_cities <- stack(cities)

#fit regression model
cities.aov <- aov(values ~ ind, data = stacked_cities)

#test model
summary(cities.aov)

#Chr 15, Q2
#make dataframe
WK1 <- c(152, 183, 151, 162, 156)
WK2 <- c(158, 151, 183, 165, 173)
WK3 <- c(188, 190, 178, 173, 180)
WK4 <- c(167, 166, 182, 167, 180)

All_WK <- data.frame(WK1, WK2, WK3, WK4)

stacked_WKS <- stack(All_WK)

#fit regression model
All_WKS.aov <- aov(values ~ ind, data = stacked_WKS)

#test model
summary(All_WKS.aov)

#Chr 15, Q3
#make dataframe
Area1 <- c(15,22,13,22,14,13,15,21)
Area2 <- c(17,8,22,22,9,10,9,13)
Area3 <- c(20,25,17,20,14,21,10,23)

Area <- data.frame(Area1, Area2, Area3)

#test the dataframe
head(Area)
summary(Area)

Stacked_Area <- stack(Area)
Stacked_Area

#fit regression model
Area.aov <- aov(values ~ ind, data = Stacked_Area)

#view output of model
summary(Area.aov)

#Chr 15.2
pf(6.70, 3, 9, lower.tail = F)
pf(0.15, 3, 12, lower.tail = F)
pf(6.90, 3, 9, lower.tail = F)
pf(10.75, 2, 6, lower.tail = F)
pf(0.46, 3, 12, lower.tail = F)
pf(7.75, 2, 6, lower.tail = F)
pf(5.20, 2, 6, lower.tail = F)
pf(0.77, 3, 9, lower.tail = F)
pf(5.62, 3, 9, lower.tail = F)
pf(5.04, 3, 12, lower.tail = F)
pf(0.29, 3, 12, lower.tail = F)

#Ch15.3
pf(5.15, 2, 18, lower.tail = F)
pf(2.98, 2, 18, lower.tail = F)
pf(2.78, 1, 18, lower.tail = F)
pf(3.79, 2, 18, lower.tail = F)
pf(0.80, 3, 24, lower.tail = F)
pf(3.37, 3, 24, lower.tail = F)
pf(4.25, 2, 18, lower.tail = F)
pf(76.53, 2, 24, lower.tail = F)
pf(2.6837, 2, 18, lower.tail = F)
pf(3.53, 1, 18, lower.tail = F)
