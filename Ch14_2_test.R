#Ch14.2 Coefficient of Determination and Adjusted R-squared

#set up independent (x) & dependent (y) variables

#get R^2 for single independent variable
RSS <- 4.718345792  #residual sum of squares
TSS <- 19.87238347 	 #total sum of squares
R_sqr <- 1-(RSS/TSS)

R_sqr_final <- round(R_sqr,4)
R_sqr_final

#adjusted R sqr: use this to explain proportion of variation
#for multiple independent variables
n <- 7  #sample size (n) is total df + 1
k <- 1   #numer of independent variables in regression model
RSS <- 4.718345792 #residual means square
TSS <- 19.87238347 #Total means square
adj_R_sqr <- 1-(RSS/TSS)*(n-1)/(n-(k+1))
adj_R_sqr
round(adj_R_sqr, 4)

#get percent of variation
SSE <- 626824.0170  #sum of squares residual/error 
TSS <- 983885.4399  #sum of total
per_var <- (SSE/TSS) * 100
round(per_var,2) 

