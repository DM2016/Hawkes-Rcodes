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

x <- c(80,64,58,45,77,20,23,29,53,50,65,41)
y <- c(113,150,159,178,139,224,203,149,167,109,102,291)

test_stat <- cor.test(x,y, method = "spearman")
test_stat

#test null hyp
ZaTwo <- 0.794  #alpha = 0.01, n= 10
n <- length(x)
n
z_critical <- ZaTwo/sqrt(n-1)
z_critical

#reject null hyp if test stat >= than critical val
#fail to reject null hyp, if test stat < critical val

###########################
x <- c(80,49,58,44,64,77,66,37,36,50,43,74)
y <- c(152,212,257,196,217,181,198,253,215,250,274,160)

test_stat <- cor.test(x,y, method = "spearman")
test_stat

#test null hyp
ZaTwo <- 0.497  #alpha = 0.1, n= 12
n <- length(x)
n
z_critical <- ZaTwo/sqrt(n-1)
z_critical
