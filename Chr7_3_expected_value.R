#Chr7.3
#get expected value
x <- c(0,1,2,3,4,5,6,7,8)
PXx <- c(1/9,1/9,1/9,
         1/9,1/9,1/9,
         1/9,1/9,1/9)

EX <- data.frame(x, PXx)
EX1 <- EX %>%
  mutate(
    expected = (x*PXx),
    x_sqr_PXx = (x^2*PXx)
  )
EX1 
EX1sumA <- sum(EX1$expected)
EX1sumA
EX1sumB <- sum(EX1$x_sqr_PXx)
EX1sumB

#get variance
my_var <- EX1sumB-EX1sumA^2
my_var

#get std
sqrt(my_var)

###
#chr 7.4
(1/15)*0.6^4

