#example code:
#Response_Time <- c('6.51-8.50', '8.51-10.50',
#                   '10.51-12.50', '12.51-14.50', '14.51-16.50')

#Response_Time <- c('7-7.9', '8-8.9', '9-9.9', '10-10.9',
#                   '11-11.9', '12-12.9', '13-13.9')

my_class0 <- c('51-58','59-66','67-74','75-82','83-90')

#Days <- c('1-5', '6-10', '11-15', '16-20', '21-25', '26-30')

midpoint <- c(54.5, 62.5, 70.5, 78.5, 86.5)
freq <- c(7,13,11,5,3)

Grouped_Data1 <- data.frame(my_class0, midpoint, freq)
Grouped_Data1

Grouped_data2 <- Grouped_Data1 %>%
  mutate(
    freq_midpt = freq*midpoint,
    freq_midpt_sqt = freq*(midpoint^2)
  )

freq_sum <- sum(Grouped_data2$freq)
freq_sum
freq_midpt_sum <- sum(Grouped_data2$freq_midpt)
freq_midpt_sum

#get mean
my_mean <- freq_midpt_sum/freq_sum
my_mean

###########
freq_midpt_sqt_sum <- (sum(Grouped_data2$freq_midpt_sqt))
freq_midpt_sqt_sum

####For Pop calculations
pop_mean1 <- freq_midpt_sqt_sum/freq_sum
pop_mean1

pop_mean2 <- (freq_midpt_sum/freq_sum)
pop_mean2

pop_var <-  pop_mean1 - (pop_mean2^2)
pop_var
sqrt(pop_var)

######For sample calculations

sample_mean1 <- freq_midpt_sqt_sum/(freq_sum-1)
sample_mean1

sample_mean2 <- ((freq_midpt_sum^2/freq_sum))/(freq_sum-1)
sample_mean2

sample_var <-  sample_mean1 - (sample_mean2)
sample_var
sqrt(sample_var)

