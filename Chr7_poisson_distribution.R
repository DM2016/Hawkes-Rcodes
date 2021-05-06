#ch7.5 Poisson Distribution
#P(x; μ) = (e^-μ) (μ^x) / x!

#inputs
u <- 7
x <- 4

#poisson for loop
my_poisson <- c()
for (entry in x) {
  #get the number for each entry in X
  poisson0 <- (exp(-u)*(u^x))/factorial(x)
  my_poisson <- c(poisson0)
  #get the total of all entries
  my_poisson1 <- sum(my_poisson)
}


#test outputs
my_poisson
my_poisson1

#complementary prob
my_poisson2 <- 1-my_poisson1
my_poisson2
