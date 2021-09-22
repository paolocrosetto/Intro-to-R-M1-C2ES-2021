# TD 1 

install.packages("nycflights13")
library ("nycflights13")
# View("nycflights13")
nycflights13::airlines
df= flights
View(df)
summary(flights) # stats descriptives

install.packages("skimr")
skimr::skim(df) # ne foncitonne pas, fait des histogrammes des variables

install.packages("tidyverse")
library(tidyverse)
#table(df, df$)
filter(df, month == 7 & day == 7 & origin == "JFK" & year = 2013)
filter(df, month %in% c(3,4,5,6))
select(df, tailnum) # numéro de vol

# arrange pour trier les données
arrange(df, sched_dep_time) # mettre un - devant la variable pour ordonner
# dans l'ordre décroissant 

# select pour sélectionner des variables
select(df, origin, dest)
select(df, starts_with("arr"))
# avec contain
select(df, contains("t")) # une variable avec t

# solution optimale avec pipe
df %>% # et ensuite ==> CtrlMajM
  filter(df, origin == "JFK" & month == 5 & day == 4 ) %>% 
  select(dest)

# nouvel exo 
df %>%
  filter(dep_time >= 1200 & dep_time <=1400 & month == 7 & day ==3) %>% 
  arrange(origin) %>% 
  select(dep_time, arr_time)

# fonction rename
# rename (df, mois = month)
rename(df, flight = vol)

# mutate ==> change le contenu des variables en créant des nouvelles variables
df %>% mutate(year_from_now = 2021 - year) %>% 
  select(year, year_from_now)

df %>%
  mutate(year_from_now = 2021 - year, 
         speed= distance / air_time , speed)
select(year_from_now, distance, air_time, speed)

# vitesse converti
df %>% 
  mutate(mph= (distance/(1/60*air_time)),
         kmh= mph*1.6) %>% 
select(mph,kmh)
  
# summarise (pour chaque groupe) : prend un vecteur , retourne une valeur
# retard moyen des avions
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE),
            mean_delay_arrive = mean(arr_delay, na.rm = T),
            sd_delay_depart = sd(dep_delay, na.rm = T))

# group_by : grouper les obvs

df %>% 
  group_by(origin)
df %>% 
  group_by(origin, month)
# combinaison de summarise et group_by
# quel est le retard moyen des aéroports ?
df %>% group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay, na.rm))

# quel est la vitesse moyenne des vols partant entre 11h et 13h par mois ?
df %>% 
  mutate(mph= (distance/(1/60*air_time)),
         kmh= mph*1.6) %>% 
  filter(dep_time >= 1200 & dep_time <=1300) %>% 
  group_by(month) %>% 
summarise(meanspeedflight= mean(dep_delay, na.rm = T))

# quel est le delai moyen des vols par chaque compagnie par mois

df %>% 
  group_by(carrier,month) %>% 
  summarise(moyenne=mean(dep_delay, na.rm = T)) # pas besoin de select
# c'est un select en plus fort où on peut appliquer une fonction

# question 2 sur diapo
df %>% 
  group_by(origin,month,day) %>% 
  summarise(max(dep_delay, na.rm= T))
# question 3 sur diapo
df %>% 
  group_by(origin) %>% 
  summarise(meanheure= mean(air_time, na.rm = T))
###################################################
# babynames !!!
install.packages("babynames")
library(babynames)
df2 = babynames
View(df2)
df2 %>% filter(year == 1999, sex == 'M') %>% 
  group_by(name)
  