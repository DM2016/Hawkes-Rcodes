#chr 6 intro prob

Males <- c(58, 113, 172, 158, 99)
Females <- c(196, 260, 174, 261, 272)

Male_sum <- sum(Males)
Female_sum <- sum(Females)

total_sum <- Male_sum+Female_sum

Males[3]/Male_sum
Males[3]/total_sum

#Chr6.3

coins <- c(14,20,14,29)
coin_sum <- sum(coins)

pcoins1 <- coins[4]/coin_sum
pcoins2 <- coins[3]/(coin_sum-1)
my_prob1 <- pcoins1*pcoins2
my_prob1

##cards
#with replacement
cards1 <- (1/4)*(1/2)*(1/13)
cards1

#without replacement
(4/(52-1))*(4/(52))
(4/52)*(3/51)
(9/27)*(8/26)

#physics
male_class <- c(17,14,9,9)
male_class1 <- sum(male_class)
male_class[4]/male_class1

###
#Chr 6 combinations
library("combinat")

combn(13,4)
choose(13, 4) == ncol(combn(13, 4))
choose(13, 4)

#combo santity check
combo_num <- 13
combo_r <- 4
my_combo <- factorial(combo_num)/(factorial(combo_r)*factorial(combo_num - combo_r))
my_combo

###get permutations
num <- 15
r <- 4
permutations <- factorial(num)/factorial(num - r)
permutations

##candidate positions
candidate_num <- 11
candidate_r <- 6
candidate_permutations <- factorial(candidate_num)/factorial(candidate_num - candidate_r)
candidate_permutations

##DJ Raven is making a playlist for a website;
#she is trying to decide what 6 songs to play
#and in what order they should be played.
#If she has her choices narrowed down to
#11 country, 16 rock, and 19 blues songs,
#and she wants to play an equal number of country,
#rock, and blues songs, how many different playlists are possible?
##combinations for music
country <- choose(11, 2)
rock <- choose(16, 2)
blues <- choose(19, 2)

#permutations for all music
perm_num <- 6
perm_r <- 6
perm_permutations <- factorial(perm_num)/factorial(perm_num - perm_r)
perm_permutations

#can do with factorial(6) also

#final
total_music_lineup <- perm_permutations*country*rock*blues
total_music_lineup
formatC(total_music_lineup, format = "e", digits = 2)


#DJ titus
pop <- choose(21,2)
country <- choose(18,2)
hip_hop <- choose(9,2)

DJ_titus_lineup <- factorial(6)*pop*country*hip_hop
DJ_titus_lineup
formatC(DJ_titus_lineup, format = "e", digits = 2)
####

length(c('C','o','e','f','f','i','c','i','e','n','t'))

#factorial
factorial(7)
factorial(8)/factorial(5)

#quilt
choose(11,4)
choose(7,3)
choose(5,4)
quilt <- choose(11,4)*choose(7,3)*choose(5,4)
quilt

#pizza
5*choose(4, 2)*choose(8,4)

#books
#An English teacher needs to pick 3 books 
#to put on his reading list for the next school year.
#He has narrowed down his choices to 19 novels and 18 plays.
#How many different ways can he choose the books
#to put on the list?

choose(37,3)

# An English teacher needs to pick 3 books
# to put on his reading list for the next school year.
# He has narrowed down his choices to 19 novels and 18 plays.
# How many different ways can he choose
# the books to put on the list
# if he wants to include at least one play?

#this means no more than 2 novels
book_combo1 <- choose(19,2) * choose(18,1)
book_combo2 <- choose(19,1) * choose(18,2)
book_combo3 <- choose(19,0) * choose(18,3)

total_book_combo <- book_combo1 + book_combo2 + book_combo3
total_book_combo

##game outcomes
#coin, dice, card
(2^3)*(6^2)*choose(52,4)

#repeated coin odds
choose(14,9)+choose(14,10)+choose(14,11)+choose(14,12)
choose(11,7)+choose(11,8)+choose(11,9)+choose(11,10)

##composer
composer_num <- 12
composer_r <- 5
composer_permutations <- factorial(composer_num)/factorial(composer_num - composer_r)
composer_permutations

##division P/C
my_num <- 10
my_r <- 7
my_permutations <- factorial(my_num)/factorial(my_num - my_r)
my_permutations

choose(10, 5)

my_permutations/choose(10, 5)

##division C/P
my_num <- 13
my_r <- 6
my_permutations <- factorial(my_num)/factorial(my_num - my_r)
my_permutations

choose(13, 7)
choose(13, 7)/my_permutations

####

#girls/boys sports
#5 girls, 5 boys, alternating, find all line up combos
lineup <- factorial(5)*factorial(5) * 2
lineup

#phonelines
#There are 9 people in an office with 3 different phone lines.
#If all the lines begin to ring at once,
#how many groups of 3 people can answer these lines?
choose(9, 3)

#english

office_num <- 12
office_r <- 7
office_permutations <- factorial(office_num)/factorial(office_num - office_r)
office_permutations

#child/adult sports teams combos
adults1 <- choose(17,2)
adults2 <- choose(17,3)
adults3 <- choose(17,4)

kids1 <- choose(15,2)
kids2 <- choose(15,1)
kids3 <- choose(15,0)

adult_kid1 <- adults1*kids1
adult_kid2 <- adults2*kids2
adult_kid3 <- adults3*kids3

total_adult_kid_combo <- adult_kid1 + adult_kid2 + adult_kid3
total_adult_kid_combo

#english 2
#An English teacher needs to pick 10 books
# to put on his reading list for the next school year,
# and he needs to plan the order in which they should be read.
# He has narrowed down his choices to 5 novels, 6 plays, 6 poetry books,
# and 8 nonfiction books. If he wants to include no more
# than 2 nonfiction books, how many different reading schedules
# are possible?
#5 novels + 6 plays + 6 poetry = 17 books
# need each scenario: 8Cx, 17Cy, no more than 2 nonfictions
no_nonfiction <- choose(8,0)*choose(17,10)
one_nonfiction <- choose(8,1)*choose(17, 9)
two_nonfiction <- choose(8,2)*choose(17, 8)

#number of permutations for all books
factorial(10)

total_nonfiction <- factorial(10)*(no_nonfiction+one_nonfiction+two_nonfiction)
total_nonfiction

##for teacher wanting to use all 6 plays
total_plays <- choose(6,6)*choose(19,4)*factorial(10)
total_plays
formatC(total_plays, format = "e", digits = 2)

#english 3
#nine books for school line up
#7 novels + 3 plays + 4 poetry + 4 nonfiction
#no more than 3 novels
#7 novels total, 11 others
# need each scenario: 7Cx, 11Cy, no more than 3 novels
no_novels <- choose(7,0)*choose(11,9)
one_novels <- choose(7,1)*choose(11, 8)
two_novels <- choose(7,2)*choose(11, 7)
three_novels <- choose(7,3)*choose(11, 6)

#number of permutations for all books
factorial(9)

total_novels <- factorial(9)*(no_novels+one_novels+two_novels+three_novels)
total_novels
formatC(total_novels, format = "e", digits = 2)
##for teacher wanting to use all 4 poetrys
total_poetrys <- choose(4,4)*choose(14,5)*factorial(9)
total_poetrys
formatC(total_poetrys, format = "e", digits = 2)
  
##for teacher wanting to use all 6 plays
total_plays <- choose(6,6)*choose(19,4)*factorial(10)
total_plays
formatC(total_plays, format = "e", digits = 2)

#Jack at the movies
choose(30,4)

#jack at the movies 2
#
one_foreign <- choose(10,1)*choose(20, 3)
two_foreign <- choose(10,2)*choose(20, 2)
three_foreign <- choose(10,3)*choose(20, 1)
four_foreign <- choose(10,4)*choose(20, 0)

foreign_movies <- (one_foreign+two_foreign+three_foreign+four_foreign)
foreign_movies

#wayne at the movies
three_dramas <- choose(7,3)*choose(16, 0)
two_dramas <- choose(7,2)*choose(16, 1)
factorial(3)

wayne_movie_lineup <- (three_dramas+two_dramas)
wayne_movie_lineup
