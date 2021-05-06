#ch17.3 Wilcox rank sum test
#n1 smaller of sample sizes
#n2 larger of sample sizes
#for n1 less than 10
#input the data
Program_A <- c(250,150,210,130,100,170)	
Program_B <- c(100,190,180,160,230,180)

l1 <- lengths(list(Program_A, Program_B))

n1 <- min(l1)
n2 <- max(l1)


#make dataframe
Program_data <- data.frame( 
  sections = c(rep("Program_A", length(Program_A)),
               rep("Program_B", length(Program_B))),
  scores = c(Program_A, Program_B)
)  

Program_data

#carry out function
Program_data1 <- Program_data %>%
  mutate(
    score_rank = rank(scores)
  ) %>%
  group_by(sections) %>%
  summarise(test_stat = sum(score_rank))

Program_data1


#################################
# for n1 greater or equal to 10
#Tx is rank sum of sample with fewest members
##if sample sizes are the same, 
##Tx= the rank sum of the population hypothesized to be shifted to the right if Ha >
##if Ha !=, and if sample sizes are the same, use either rank sum
##if Ha < Tx=  the rank sum of the population hypothesized to be shifted to the left
## reject null hyp if critical value > your test value
## fail to reject null hyp if critical value < your test value

Program_A <-	c(78,96,81,79,89,87)			
Program_B <-	c(87,96,100,91,93,97,99,95)

l1 <- lengths(list(Program_A, Program_B))

n1 <- min(l1)
n2 <- max(l1)

#make dataframe
Program_data <- data.frame( 
  sections = c(rep("Program_A", length(Program_A)),
               rep("Program_B", length(Program_B))),
  scores = c(Program_A, Program_B)
)  

Program_data

#carry out function
Program_data1 <- Program_data %>%
  mutate(
    score_rank = rank(scores)
  ) %>%
  group_by(sections) %>%
  summarise(test_stat = sum(score_rank))

Program_data1
# sections  test_stat
# <chr>         <dbl>
# 1 Program_A     27
# 2 Program_B     78

Tx <- 27 #using the smallest value
n1 
n2 

z <- (Tx - (n1*(n1+n2+1))/2)/sqrt((n1*n2*(n1+n2+1))/12)
z
##################################
Program_A <-	c(87,71,79,75,92,87)			
Program_B <-	c(102,97,93,82,77,84,75,67)

l1 <- lengths(list(Program_A, Program_B))

n1 <- min(l1)
n2 <- max(l1)

#make dataframe
Program_data <- data.frame( 
  sections = c(rep("Program_A", length(Program_A)),
               rep("Program_B", length(Program_B))),
  scores = c(Program_A, Program_B)
)  

Program_data

#carry out function
Program_data1 <- Program_data %>%
  mutate(
    score_rank = rank(scores)
  ) %>%
  group_by(sections) %>%
  summarise(test_stat = sum(score_rank))

Program_data1
# sections  test_stat
# <chr>         <dbl>
# 1 Program_A     129
# 2 Program_B     249

Tx <- 129 #using the smallest value
n1 
n2 

z <- (Tx - (n1*(n1+n2+1))/2)/sqrt((n1*n2*(n1+n2+1))/12)
z

############################################

Program_A <- c(160,150,280,260,210,200)	
Program_B <- c(290,100,110,150,210,130)

l1 <- lengths(list(Program_A, Program_B))

n1 <- min(l1)
n2 <- max(l1)


#make dataframe
Program_data <- data.frame( 
  sections = c(rep("Program_A", length(Program_A)),
               rep("Program_B", length(Program_B))),
  scores = c(Program_A, Program_B)
)  

Program_data

#carry out function
Program_data1 <- Program_data %>%
  mutate(
    score_rank = rank(scores)
  ) %>%
  group_by(sections) %>%
  summarise(test_stat = sum(score_rank))

Program_data1

