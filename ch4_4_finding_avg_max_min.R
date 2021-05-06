#Ch4.4
Beer0 <- read.csv('D:/Docker/Hawkes_Rcodes/Beers_and_Breweries.csv', skip =3, header = T)


UBC <- Beer0 %>%
  filter(brewery.name == 'Upslope Brewing Company')

UBC_mean <- mean(UBC$abv)
UBC_sd <- sd(UBC$abv)
coefficient.variation(UBC_sd, UBC_mean)

San_Diego_Beer <- Beer0 %>%
  filter(city == 'San Diego')

max(San_Diego_Beer$abv)

TailGate_Beer <- Beer0 %>%
  filter(brewery.name == 'TailGate Beer')

length(TailGate_Beer$brewery.name)

IPAs <- TailGate_Beer %>%
  filter(str_detect(beer.style, 'IPA'))

#########################################
#for houses
Houses0 <- read.csv('D:/Docker/Hawkes_Rcodes/Mount_Pleasant_Real_Estate_Data.csv', header = T)

Prices1 <-Houses0%>%
  filter(Acreage >= 0.21 & Acreage <= 0.30) %>%
  as.numeric(gsub("[\\$,]", "", List.Price))

min(Prices1$List.Price)

Prices2 <-  as.numeric(gsub("[\\$,]", "", List.Price))

Prices2_mean <- mean(Prices2)

