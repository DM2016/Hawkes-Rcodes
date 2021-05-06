###################
#chr 17.2 Wilcox Signed Ranked Test
library(tidyverse)


#Ha: >, then T=T+= the sum of the ranks of the positive differences.
#Ha: ≠, then T=Min(T+,T−).
#Ha: <, then T=T−= the sum of the ranks of the negative differences.

#Reject Null hyp if test stat less than or equal to critical value

#use this equation if n <= 25 

#n <- 
#Test_stat <-

z <- (Test_stat - (n*(n+1))/4)/sqrt((n*(n+1)*(2*n+1))/24)

#If n≤25, reject H0 if T≤Tc, the critical value in the table.
#If n>25, and
##Ha: > or Ha: <, reject H0 if z < −zα.
##Ha: ≠, reject H0 if z < −zα/2 or if z > z/α2.

##############################################
#My Function (useful practice)
#Number of Grocery Items Processed in Three Minutes
Cashier <- c(1,2,3,4,5,6,7,8)
Old_Cash_Register <-	c(42,62,60,58,73,63,56,67)
New_Cash_Register	<- c(39,59,67,63,82,69,64,67)

Grocery0 <- data.frame(Cashier, Old_Cash_Register, New_Cash_Register)
Grocery0

Grocery1 <- Grocery0 %>%
  mutate(
    Difference = Old_Cash_Register - New_Cash_Register,
    Abs_Difference = case_when(
      Difference != 0 ~ abs(Difference),
      Difference == 0 ~ NA_real_
    ),
    Rank_Abs_Val_Diff = rank(Abs_Difference, na.last = T),
    Signed_Rank = case_when(
      Difference < 0 ~ -1*Rank_Abs_Val_Diff,
      Difference > 0 ~ 1*Rank_Abs_Val_Diff,
      Difference == 0 ~ NA_real_
    ),
    Rank_type = case_when(
        Signed_Rank == 0 ~ 0,
        Signed_Rank < 0 ~ 1, #negative numbers
        Signed_Rank > 0 ~ 2 #positive numbers
        )
  )%>%
  group_by(Rank_type) %>%
  summarise(Test_stat = sum(Signed_Rank))

Grocery1

#sum the last 
################################
#simple way:
wilcox.test(Old_Cash_Register, New_Cash_Register, paired = T, alternative = 'greater')


Old_Cash_Register_Q2 <- c(57,70,67,72,55,60,66,68,57)
New_Cash_Register_Q2 <- c(59,79,65,80,61,65,69,65,56)
wilcox.test(Old_Cash_Register_Q2, New_Cash_Register_Q2, paired = T, alternative = 'greater')
################################

Old_Cash_Register_Q3 <- c(52,43,63,56,45,54)
New_Cash_Register_Q3 <-	c(54,41,64,61,48,58)

wilcox.test(Old_Cash_Register_Q3, New_Cash_Register_Q3, paired = T, alternative = 'less')

################################
Old_Cash_Register_Q4 <- c(44,57,38,66,64,65,51)
New_Cash_Register_Q4 <- c(48,55,41,65,73,72,60)

wilcox.test(Old_Cash_Register_Q4, New_Cash_Register_Q4, paired = T, alternative = 'less')

################################

Model_A <- c(189,194,196,166,163,157,198,171)
Model_B	<- c(184,197,192,166,160,155,197,164)

wilcox.test(Model_B, Model_A, paired = T, alternative = 'two.sided')
################################

Model_A <- c(163,143,153,146,149,155)
Model_B <- c(160,146,157,147,151,160)

wilcox.test(Model_A, Model_B, paired = T, alternative = 'two.sided')

alpha <- 0.2
qsignrank(alpha, 8, lower.tail=F) 

################################

Before_Diet <- c(189,194,196,166,163,157,198,171)
After_Diet <- c(184,191,192,166,160,155,197,164)

wilcox.test(After_Diet, Before_Diet, paired = T, alternative = 'less')
#################################
Before_Diet <- c(159,159,148,174,185,190,166)
After_Diet <- c(160,155,144,169,187,182,163)

wilcox.test(After_Diet, Before_Diet, paired = T, alternative = 'less')

#################################

Before_Diet <- c(150,145,160,155,154,153)
After_Diet	<- c(148,141,158,155,151,152)

wilcox.test(After_Diet, Before_Diet, paired = T, alternative = 'less')
##################################

Model_A	 <- c(166,179,163,188,169,192,166,158,160) 
Model_B	 <- c(163,170,158,182,160,188,164,158,153)

wilcox.test(Model_B, Model_A, paired = T, alternative = 'two.sided')

###################################

Model_A	<- c(156,170,145,200,162,180,160)
Model_B	<- c(152,168,148,195,163,178,157)

wilcox.test(Model_B, Model_A, paired = T, alternative = 'two.sided')

##################################

Old_Cash_Register	<- c(66,46,56,49,52,58)
New_Cash_Register <- c(69,49,60,48,54,63)

wilcox.test(Old_Cash_Register, New_Cash_Register, paired = T, alternative = 'greater')

##################################

Model_A	<- c(174,170,159,174,193,197,180,173)
Model_B	<- c(171,165,159,170,181,199,179,169)

wilcox.test(Model_B, Model_A, paired = T, alternative = 'two.sided')
##################################

Before_Diet	<- c(148,146,160,155,152,158)
After_Diet <-	c(145,142,160,153,154,155)

wilcox.test(After_Diet, Before_Diet, paired = T, alternative = 'greater')
##################################

Model_A	<- c(191,173,171,159,155,169,199,193) 
Model_B	<- c(185,175,167,164,150,169,198,196)

wilcox.test(Model_B, Model_A, paired = T, alternative = 'two.sided')
##################################

Before_Diet	<- c(159,170,194,161,172,163,163,178,173)
After_Diet <-	c(151,165,193,155,173,167,156,171,171)

wilcox.test(After_Diet, Before_Diet, paired = T, alternative = 'less')
##################################

Old_Cash_Register	<- c(67,45,62,43,40,48,52,61,58)
New_Cash_Register	<- c(64,51,69,49,49,55,57,61,68)

wilcox.test(Old_Cash_Register, New_Cash_Register, paired = T, alternative = 'greater')

##################################
Old_Cash_Register	<- c(68,72,65,53,62,74,69,77,65)
New_Cash_Register	<- c(71,70,66,53,66,79,75,71,73)

wilcox.test(Old_Cash_Register, New_Cash_Register, paired = T, alternative = 'greater')

##################################

Old_Cash_Register	<- c(53,48,63,58,57,56)
New_Cash_Register	<- c(51,52,65,58,60,57)

Register0 <- data.frame(Old_Cash_Register, New_Cash_Register)

####################
#Hard way/Sanity check
Register1 <- Register0 %>%
  mutate(
    Differences = (Old_Cash_Register - New_Cash_Register),
    Abs_Diff_Val = abs(Differences),
    Rank_Abs_Val_Diff = case_when(
      any(Abs_Diff_Val==0)~(rank(Abs_Diff_Val)-1),
      any(Abs_Diff_Val>0)~(rank(Abs_Diff_Val)),
      any(Abs_Diff_Val<0)~(rank(Abs_Diff_Val))
    ),
    Sign = case_when(
      Differences > 0 ~ 'pos',
      Differences < 0 ~ 'neg',
      Differences == 0 ~ NA_character_
    )
  )

Register1

Register1_tbl <- table(Register1$Sign, useNA = 'always')
Register1_tbl

Register2 <- Register1 %>%
  group_by(Sign) %>%
  summarise(sum(Rank_Abs_Val_Diff))

Register2
#######################

Old_Cash_Register	<- c(54,67,48,66,58,75,61)
New_Cash_Register	<- c(55,69,44,74,64,78,65)

wilcox.test(Old_Cash_Register, New_Cash_Register, paired = T, alternative = 'greater')
#######################################

Before_Diet	<- c(166,179,163,188,169,192,166,158,160)
After_Diet <-	c(164,170,158,182,160,188,168,158,153)

wilcox.test(After_Diet, Before_Diet, paired = T, alternative = 'less')
