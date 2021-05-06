#Ch17.5 Runs Tests Randomness

library(randtests)

#runs.test(x, exact = FALSE, alternative 
#= c("two.sided", "less", "greater"))

x0 <- 'HHHTHHTTHTHTHTHT'
N <- nchar(x0)
N
m <- str_count(x0,"H")
m
n <- str_count(x0,"T")
n
R <- 12 #how many regions in the original string

#n is greater than 20, get uR and stdR
uR <- 1 + (2*m*n)/N
uR

#stdR
stdR <- sqrt(((2*m*n)*(2*m*n-N))/((N^2)*(N-1)))
stdR

#get the test stat

z <- (R-uR)/stdR
z
###############################

y <- 'NNDNDNNNNDNDDNDNDNDNDNNDNNNNNNNDN'
N <- nchar(y)
N
m <- str_count(y,"D")
m
n <- str_count(y,"N")
n
R <- 21 #how many regions in the original string

#n is greater than 20, get uR and stdR
uR <- 1 + (2*m*n)/N
uR

#stdR
stdR <- sqrt(((2*m*n)*(2*m*n-N))/((N^2)*(N-1)))
stdR

#get the test stat

z <- (R-uR)/stdR
z

################################

#ch17.5 example 3
deaths <- c(36, 15, 9, 12, 5, 93, 41, 83, 69, 58, 37)
summary(deaths)

#count how many are above or below the median
deaths1 <- deaths[deaths >37]
deaths2 <- deaths[deaths <37]
length(deaths1)
length(deaths2)

R <-2

#if m<20 and n<20, the test statistic is R
########################################

y <- 'DDNNNDDNNNNNNNDDNDDNNNNNNNNDDDDNNN'
N <- nchar(y)
N
m <- str_count(y,"D")
m
n <- str_count(y,"N")
n
R <- 10 #how many regions in the original string

#n is greater than 20, get uR and stdR
uR <- 1 + (2*m*n)/N
uR

#stdR
stdR <- sqrt(((2*m*n)*(2*m*n-N))/((N^2)*(N-1)))
stdR

#get the test stat

z <- (R-uR)/stdR
z

pnorm(-abs(z))

##########################
deaths <- c(4,93,11,27,83,23,60,14,70,9,65)
summary(deaths)

#count how many are above or below the median
deaths1 <- deaths[deaths >27]
deaths2 <- deaths[deaths <27]
length(deaths1)
length(deaths2)

R <-10

pnorm(-abs(z))

#####################
x0 <- 'HTTHTHTHTTHTHTHTHTHT'
N <- nchar(x0)
N
m <- str_count(x0,"H")
m
n <- str_count(x0,"T")
n
R <- 18 #how many regions in the original string

#n is greater than 20, get uR and stdR
uR <- 1 + (2*m*n)/N
uR

#stdR
stdR <- sqrt(((2*m*n)*(2*m*n-N))/((N^2)*(N-1)))
stdR

#get the test stat

z <- (R-uR)/stdR
z

pnorm(-abs(z))
