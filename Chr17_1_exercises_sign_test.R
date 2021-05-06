#sign test practice
library(tidyverse)
library(qwraps2)
library(MASS)
library(BSDA)

x <- c(7.8, 6.6, 6.5, 7.4, 7.3, 7., 6.4, 7.1, 6.7, 7.6, 6.8)

#compute two sided sign test for hull hyp
SIGN.test(x, md = 6.5, conf.level = 0.99) #is the median 6.5?

#Hawkes test
#Can the researcher conclude at α=0.01 that the median reaction time 
#is longer after consuming one ounce of 100-proof alcohol?

#sign differences
HK <- c(-0.1,0.0,-0.1,-0.2,-0.1,
        0.0,-0.1,-0.2,-0.2,-0.1)

SIGN.test(HK, md = 0,alternative = "greater", conf.level = 0.99)

#############################

#sign test for n > 25
chi <- 480
n <- 1000

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z
#-1.23329
#-1.65 is the critical value
# z is greater than the critical value at a = 0.05
# therefore we fail to reject null hyp
#############################
#sign test for n > 25
chi <- 150
n <- 1100

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z
z_crit <- qnorm(0.05, lower.tail=T)
z_crit
#############################

#sign test for n > 25
chi <- 200
n <- 1200

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z

#this is less than, so use a, which is a = 0.05 (critical value is -1.65)
####################################

#sign test for n > 25
chi <- 350
n <- 1100

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z

#-10.6637
#-2.33 at a = 0.01
#z is less than the critical value
#therefore we will reject the null hyp

#####################################

#Braking Distance of High-End Sedans (in Feet)
Driver <- c('Driver_1','Driver_2','Driver_3','Driver_4','Driver_5',
            'Driver_6','Driver_7','Driver_8','Driver_9')
Model_A <- c(180,200,171,165,185,172,199,184,170)
Model_B <- c(181,187,163,162,185,184,199,150,175)

Sedans <- data.frame(Driver, Model_A, Model_B)

Sedans1 <- Sedans %>%
  mutate(
    Differences = (Model_B - Model_A),
    Sign = case_when(
      Differences > 0 ~ 'pos',
      Differences < 0 ~ 'neg',
      Differences == 0 ~ NA_character_
    )
  )

Sedans1

Sedans_tbl <- table(Sedans1$Sign, useNA = 'always')
Sedans_tbl
#count how many positive and negative signs there are,
#use value of smaller number of pos/negs for test stat
#reject null hyp if test stat less than critical val (for one tailed)
###########################
#cholesterol
subjects <- c('Subject_1', 'Subject_2', 'Subject_3', 'Subject_4', 'Subject_5',
              'Subject_6', 'Subject_7', 'Subject_8', 'Subject_9')

Before_Diet <- c(159,170,194,161,172,163,156,178,173)
After_Diet <- c(164,168,194,157,172,161,152,173,173)

my_diet <- data.frame(subjects, Before_Diet, After_Diet)

my_diet1 <- my_diet %>%
  mutate(
    Differences = (After_Diet - Before_Diet),
    Sign = case_when(
      Differences > 0 ~ 'pos',
      Differences < 0 ~ 'neg',
      Differences == 0 ~ NA_character_
    )
  )

my_diet1

my_diet_tbl <- table(my_diet1$Sign, useNA = 'always')
my_diet_tbl

#1
#1.644854 critical val
###########################
#Ch17 certification 
#Q1

#sign test for n > 25
chi <- 200
n <- 1200

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z

###########################
#Q7

#sign test for n > 25
chi <- 300
n <- 1200

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z

##################

#sign test for n > 25
chi <- 300
n <- 1200

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z
########################
chi <- 520
n <- 1100

#
z <- (chi + 0.5 -(n/2))/(sqrt(n)/2)
z
