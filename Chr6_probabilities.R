#Chr6.5
#failed online class (1/25)
#failed face to face (3/50)
#.73
#.27
#general equation = ((PA|B)*(PB))/((PA|B1)*P(B1)+(PA|B2)*P(B2)+(PA|Bk)*PBk)

failed_online <- ((1/25)*(0.10))/((.10*(1/25))+(0.9*(3/50)))
failed_online

#############
p_top <- (0.06*0.80)
p_bottom <- (0.06*0.80)+(0.94*0.30)

p_top/p_bottom

p_top <- (0.6*0.05)
p_bottom <- (0.6*0.05)+(0.1*0.95)
p_top/p_bottom

#politician probabilities
republicans <- 0.26
democrats <- 0.60
independents <- 0.14

ct_reform_r <- 0.23
ct_reform_d <- 0.44
ct_reform_i <- 0.33

total_rep <- ct_reform_r*republicans
total_dem <- ct_reform_d*democrats
total_ind <- ct_reform_i*independents

total_dem/sum(c(total_rep, total_dem, total_ind))

#disease probabilities
#cancer 0.5% incident rate
#cancer test fdr 8%
#cancer accuracy 96%
#0.995 true cancer rate
p_disease_top <- (0.011)*(0.98)
p_disease_bottom <- (0.011)*(0.98)+(0.989)*(0.10)

p_disease_top/p_disease_bottom


