#Ch15 final/practice
pf(5.01, 3, 12, lower.tail = F)
pf(3.28, 3, 24, lower.tail = F)
pf(1.41, 3, 9, lower.tail = F)
pf(3.22, 2, 18, lower.tail = F)
pf(4.54, 3, 9, lower.tail = F)
pf(0.73, 3, 12, lower.tail = F)


#make dataframe
Area1 <- c(16,17,19,11,21,17,19,12)
Area2 <- c(16,20,15,9,10,9,16,16)
Area3 <- c(17,22,24,25,17,14,14,22)

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

