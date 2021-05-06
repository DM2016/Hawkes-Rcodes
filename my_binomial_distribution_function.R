#make a custom binomial distribution function
X <- c(15)
p <- 0.6
n <- 17

my_binomial_distribution <- function(X,p,n) {
  my_count <- c()
  for (entry in X) {
    #get the number for each entry in X
    my_count0 <- choose(n,entry)*p^entry*(1-p)^(n-entry)
    my_count <- c(my_count, my_count0)
    #get the total of all entries
    my_count1 <- sum(my_count)
  return(my_count1)
  }
}

X1 <- c(16)
p1 <- 0.7
n1 <- 18

X2 <- c(0,1,2,3)
p2 <- 0.6
n2 <- 15

test1 <- my_binomial_distribution(X2,p2,n2)
test1

#test output
my_count
my_count1
