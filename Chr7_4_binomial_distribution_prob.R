#ch 7.4 problems

X <- 0:4
n <- 9
p <- 0.7

#general equation
# choose(n,x)*p^x*(1-p)^(n-x)
#for loop for binomial distribution
my_count <- c()
for (entry in X) {
  #get the number for each entry in X
  my_count0 <- choose(n,entry)*p^entry*(1-p)^(n-entry)
  my_count <- c(my_count, my_count0)
  #get the total of all entries
  my_count1 <- sum(my_count)
}

#test output
my_count
my_count1

#get the complementary probability
#subtract it from 1
my_count2 <- 1-my_count1
my_count2
