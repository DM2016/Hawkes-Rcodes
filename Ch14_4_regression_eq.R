#ch14.4 infer M reg
#make data frame
study_hours <- c(1,2,5,5,5)
gpa <- c(2,2,3,4,4)
act_score <- c(20,22,26,26,26)

ACT_effects <- data.frame(study_hours, gpa, act_score)

ACT_model <- lm(ACT_effects$act_score~ACT_effects$study_hours+ACT_effects$gpa)
ACT_model
summary(ACT_model)

my_anova <- anova(ACT_model)
my_anova

summary(my_anova)

########################

#import data
my_house_prices0 <- read.csv('D:/Docker/Hawkes_Rcodes/Ch14_4_ex3_house_data2.csv', header = T)
my_house_prices0

my_house_prices <- lm(my_house_prices0$Selling.Price~my_house_prices0$Square.Feet+
                      my_house_prices0$Number.of.Bedrooms+my_house_prices0$Age)

my_house_prices

summary(my_house_prices)

my_anova <- anova(my_house_prices)
my_anova

summary(my_anova)

########################

#import data


