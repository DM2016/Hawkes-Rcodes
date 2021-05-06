#ch7 hypergeometric distribution

#input values
N <- 14   #size of total population
n <- 3    #sample size
k <- 5    #number of successes/events of interest in the population
X <- 0  #number of successes in sample size n

#general equation for hypergeometric distribution
#(choose(k, X)*choose(N-k,n-X))/choose(N,n)

my_hgd <- c()
for (entry in X) {
  #get the number for each entry in X
  hgd0 <- (choose(k, X)*choose(N-k,n-X))/choose(N,n)
  my_hgd <- data.frame(X,hgd0)
  #get the total of all entries
  my_hgd1 <- sum(my_hgd$hgd0)
}

#test output
my_hgd
my_hgd1

#compl. probability
my_hgd2 <- 1-my_hgd1
my_hgd2
####
#Expected Value
Ex <- n*(k/N)
Ex

#variance
VX <- n*(k/N)*(1-(k/N))*((N-n)/(N-1))
VX

#std
sqrt(VX)


