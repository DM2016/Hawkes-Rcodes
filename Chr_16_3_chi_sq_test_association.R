#Chr 16.3 chi sq test association
#make matrix
#get En: (130*113)/254 = 57.83 & (130*141)/254 = 72.17
Q3C_data0 <- matrix(c(61,57.83,52,57.83,69,72.17,72,72.17),
                    ncol = 4, byrow = T)

colnames(Q3C_data0) <- c('n_Diseased','En_Diseased', 'n_Not_Diseased','En_Not_Diseased')
rownames(Q3C_data0) <- c('Vaccinated','Not_Vaccinated')
Q3C_data0 <- as.table(Q3C_data0)
Q3C_data0

#chi sq test
prod_chisq <- chisq.test(Q3C_data0)
print(prod_chisq, digits = 8)

#correct answer:
qchisq(0.025, 1, lower.tail = F)

####################################
Total_Fav <- 133 #total from col 1
Total_UnFav <- 48
Total_Neutral <- 49
Total_subj <- 230 #total participants in study
Total_age18to30 <- 108 #total from row 1
Total_age30to45 <- 53
Total_age45plus <- 69


#get favorable proportions of ages 18-30
p_fav <- Total_Fav/Total_subj
p18to30 <- Total_age18to30/Total_subj

En_fav <- Total_subj*p_fav*p18to30
En_fav

#get neutral proportions of ages 45+

p_Neutral <- Total_Neutral/Total_subj
p_45plus <- Total_age45plus/Total_subj

En_Neutral0 <- Total_subj*p_Neutral*p_45plus
En_Neutral0

prod_data1 <- matrix(c(40,34,34,
                       40,6,7,
                       53,8,8),
                     ncol = 3, byrow = T)


colnames(prod_data1) <- c('n_fav', 'n_unfav', 'n_Neu')
rownames(prod_data1) <- c('Age_18to30','Age_30to45','Age_45plus')
prod_data1 <- as.table(prod_data1)
prod_data1

prod_chisq <- chisq.test(prod_data1, p=x/sum(y))
print(prod_chisq, digits = 8)
qchisq(0.01, 4, lower.tail = F)

###############################

#make matrix
Q1C_data0 <- matrix(c(28,15,22,11,41,23,49,113),
                    ncol = 2, byrow = T)

colnames(Q1C_data0) <- c('male', 'female')
rownames(Q1C_data0) <- c('Below_25K','25Kto50K','50Kto75K', 'Above_75K')
Q1C_data0 <- as.table(Q1C_data0)
Q1C_data0

#chi sq test
prod_chisq <- chisq.test(Q1C_data0)
print(prod_chisq, digits = 8)
qchisq(0.01, 3, lower.tail = F)
