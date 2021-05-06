#ch14.6 model qualiatative variables

car_dealership0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch14_6_modelqvars_ex1.csv', header = T)
car_dealership0 

car_dealership_model <- lm(car_dealership0$Monthly.Sales~car_dealership0$Number.of.Dealerships+
                             car_dealership0$Q1+car_dealership0$Q2+car_dealership0$Q3)

car_dealership_model
summary(car_dealership_model)
my_anova <- anova(car_dealership_model)
summary(my_anova)
confint(car_dealership_model)

####################################
ceo_salary0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch14_6_modelqvars_ex2.csv', header = T)
names(ceo_salary0)

#1 if variant is present in entry, 0 if not

ceo_salary_model <- lm(ceo_salary0$Salary~ceo_salary0$Experience+ceo_salary0$Service+ceo_salary0$Industrial)

ceo_salary_model
summary(ceo_salary_model)
my_anova <- anova(ceo_salary_model)
summary(my_anova)
confint(ceo_salary_model)

####################################

house_price0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch14_6_modelqvars_ex3.csv', header = T)
names(house_price0)

#1 if variant is present in entry, 0 if not

house_price_model <- lm(house_price0$Price~house_price0$Sqft+house_price0$Brick)
house_price_model

summary(house_price_model)
my_anova <- anova(house_price_model)
summary(my_anova)
confint(house_price_model)

###################################

hourly_Wages0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch14_6_modelqvars_ex4.csv', header = T)
names(hourly_Wages0)

#1 if variant is present in entry, 0 if not

house_price_model <- lm(hourly_Wages0$Wage~hourly_Wages0$Experience+hourly_Wages0$Degree)
house_price_model

summary(house_price_model)
my_anova <- anova(house_price_model)
my_anova
summary(my_anova)
confint(house_price_model)
