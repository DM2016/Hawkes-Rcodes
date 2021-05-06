#Ch 17.4 Rank correlation

#get ranks for the data
#get correlation coefficient
#cor(x,y, method = "spearman")

#test for association/correlation between paired samples
#gives correlation coefficient and
#significance level for correlation

#cor.test(x, y, method="spearman")

#is n<= 30 ?
#if not, calculate critical value
#z_critical <- ZaTwo/sqrt(n-1)
#reject null hyp if abs(rs) > z_critical

#if yes, reject null hyp if abs(rs)
#is equal to or greater than critical value
#in Appendix A, Table L

x <- c(77,66,38,55,48,63,54,28,56,32,72,76)
y <- c(246,167,272,140,227,132,156,286,251,200,270,160)

test_stat <- cor.test(x,y, method = "spearman")
test_stat

#get critical value given alpha value 0.05
ZaTwo <- 0.591
n <- length(x)
n
z_critical <- ZaTwo/sqrt(n-1)
z_critical

#ch17 practice Q7
Old_Cash_Register_Q2 <- c(141,154,135,163,175,162,148)
New_Cash_Register_Q2 <- c(137,156,138,162,168,157,141)
wilcox.test(New_Cash_Register_Q2, Old_Cash_Register_Q2, paired = T, alternative = 'less')

#ch17 practice Q8
#Braking Distance of High-End Sedans (in Feet)
Subject <- c('Subject_1','Subject_2','Subject_3','Subject_4','Subject_5',
            'Subject_6','Subject_7','Subject_8','Subject_9')
Model_A <- c(180,200,171,165,185,172,199,184,170)
Model_B <- c(181,187,163,162,185,184,199,150,175)

Sedans <- data.frame(Subject, Model_A, Model_B)

Sedans1 <- Sedans %>%
  mutate(
    Differences = (Model_B - Model_A)
  )

Sedans1

#7 non zero differences, (4 negative (-58), 3 positive (18))