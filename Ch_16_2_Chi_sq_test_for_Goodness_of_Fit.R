#Ch 16.2 Chi sq test for Goodness of Fit

FA <- c(22,8,8,17,8,10,23,11,13,13,20,18)
pA <- c(1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12)

Accidents_res <- chisq.test(FA, p = pA)

print(Accidents_res, digits = 8)
qchisq(0.005, 11, lower.tail = F)
################################

I1 <- c(39,86,226)
pI <- c(0.5,0.3,0.2)

Insurance_res <- chisq.test(I1, p = pI)
print(Insurance_res, digits = 8)
qchisq(0.025, 2, lower.tail = F)

########################
attitude1 <- c(17.5,25,70,137.5)
p1 <- c(0.07,0.10,0.28,0.55)
attitude2 <- c(200,42.5,52.5,135)
p2 <- c(0.08,0.17,0.21,0.54)

attitude <- data.frame(attitude1,p1,attitude2,p2)
attitude_tbl <- table(attitude$attitude1,attitude$attitude2)
attitude_res <- chisq.test(attitude1, attitude2)
attitude_res

#use the second (observed) set of observations, with the first (expected) set of proportions
#attitude_res <- chisq.test(attitude2, p = c(0.21,0.22,0.13,0.44))
attitude_res <- chisq.test(attitude2, p = p1)

print(attitude_res, digits = 9)

qchisq(0.005, 3, lower.tail = F)
