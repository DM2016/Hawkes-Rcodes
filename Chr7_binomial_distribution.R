#ch7.4
# The Magazine Mass Marketing Company
# has received 15 entries in its latest
# sweepstakes. They know that the probability
# of receiving a magazine subscription order
# with an entry form is 0.6. What is the probability that
# no less than 4 of the entry forms will include an order?

#each trial is identical and independent

X <- c(0,1,2,3)
p <- 0.6
n <- 15

#general equation
# choose(n,x)*p^x*(1-p)^(n-x)

#this gives you the probability for less than 4 entries
my_count <- c()
for (entry in X) {
  #get the number for each entry in X
  my_count0 <- choose(n,entry)*p^entry*(1-p)^(n-entry)
  my_count <- c(my_count, my_count0)
  #get the total of all entries
  my_count1 <- sum(my_count)
}

#take the probabilty for less than 4 entries
#subtract it from 1
my_count
my_count1
my_count2 <- 1-my_count1
my_count2
##################################################
#alternatively, we could take the numbers directly
X2 <- 4:15
p <- 0.6
n <- 15

#this gives you the probability for 4 or more entries
my_countA <- c()
for (entry2 in X2) {
  #get the number for each entry in X
  my_countB <- choose(n,entry2)*p^entry2*(1-p)^(n-entry2)
  my_countA <- c(my_countA, my_countB)
  #get the total sum of all entries
  my_countC <- sum(my_countA)
}

#test your variables
my_countA
my_countC