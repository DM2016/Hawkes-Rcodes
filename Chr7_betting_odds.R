#Chr7.1
#get expected value
x <- c(2,3,4,5,6)
PXx <- c(0.2,0.2,0.2,0.1,0.3)

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

######
# Suppose that you and a friend are playing cards
# and you decide to make a friendly wager.
# The bet is that you will draw two cards without
# replacement from a standard deck. If both cards are diamonds,
# your friend will pay you $810.
# Otherwise, you have to pay your friend  $49

#get first and second cards, no replacement, both diamonds
card1 <- 13/52
card2 <- 12/51

#odds of winning
p_winning <- card1 * card2
win_payoff <- 7

#odds of losing
p_losing <- 1-p_winning
lose_penalty <- -2

#calculate the prize money
prize <- (p_winning*win_payoff) + (p_losing*lose_penalty)
prize

#if you make this bet 543 times
my_times <- 543
new_prize <- my_times*prize
new_prize
############################

#Free throws
made_ft <- 215
attempted_ft <- 322

#find prize/penalty
p_win <- (made_ft/attempted_ft)^2
win_payoff <- 32
p_lose <- 1-p_win
lose_penalty <- -24

prize <- (p_win*win_payoff) + (p_lose*lose_penalty)
prize

#play this game 590 more times
my_times <- 590
new_prize <- my_times*prize
new_prize
###############################
#Flip a coin 12 times. If you get 10 heads or less,
#I will pay you $24. Otherwise you pay me  $1252

#get odds
times <- 12
coin_flips <- 2^12

#we really just want 10 to 12 times
ten_times <- choose(12,10)
eleven_times <- choose(12, 11)
twelve_times <- choose(12, 12)

p_10 <- ten_times/coin_flips
p_11 <- eleven_times/coin_flips
p_12 <- twelve_times/coin_flips

#get odds and prize money
p_lose <- sum(c(p_10, p_11, p_12))
round(p_lose,4)
lose_penalty <- -1252
p_win <- 1-p_lose
round(p_win, 4)
win_payoff <- 24

prize <- (round(p_win,4)*win_payoff) + (round(p_lose,4)*lose_penalty)
prize

#play this game 911 more times
my_times <- 911
new_prize <- my_times*round(prize, 2)
new_prize

#Flip a coin 12 times. If you get 7 heads or less,
#I will pay you $40. Otherwise you pay me  $170

#get odds
times <- 11
coin_flips <- 2^times

#get odds
#eight_times <- choose(12,8)
nine_times <- choose(11,9)
ten_times <- choose(11,10)
eleven_times <- choose(11, 11)
#twelve_times <- choose(12, 12)

#p_8 <- eight_times/coin_flips
p_9 <- nine_times/coin_flips
p_10 <- ten_times/coin_flips
p_11 <- eleven_times/coin_flips
#p_12 <- twelve_times/coin_flips

p_lose <- sum(c(p_9, p_10, p_11))
round(p_lose,4)
lose_penalty <- -16
p_win <- 1-round(p_lose,4)
round(p_win, 4)
win_payoff <- 2


prize <- (p_win*win_payoff) + (round(p_lose,4)*lose_penalty)
prize

#if i played 712 times more
my_times <- 712
new_prize <- my_times*round(prize, 2)
new_prize

##########################################
###Plan A
firstA <- sum(rep(-15000,25))
secondA <- sum(rep(55000,11))
thirdA <- sum(rep(95000, 64))

total_planA <-sum(c(firstA, secondA, thirdA))
total_planA

###Plan B
firstB <- sum(rep(-5000,20))
secondB <- sum(rep(10000,49))
thirdB <- sum(rep(95000, 31))

total_planB <-sum(c(firstB, secondB, thirdB))
total_planB
