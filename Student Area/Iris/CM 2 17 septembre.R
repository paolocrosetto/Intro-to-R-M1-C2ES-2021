# cours du 17 septembre 

nycflights13::flights
library(nycflights13)

#import data in our space
df <- flights   #assigné  à une var locale ; trouvable dans environnement

#stat descriptive
summary(flights)   #pour chaque var on a max, min, quartiles, mediane, moyenne

#exploration des données avec skimr
install.packages("skimr")
skimr::skim(df)  #autre façon de montrer les mm données (avec historgamme)

library(tidyverse)
library(dplyr)

#filtrer
filter(df, month== 6) #une condition
filter(df, month ==6  & day == 1) #deux condition

#nom variables
names(df)

#tabulate data
table(df$origin)

#combien de vols sont partis de JFK le 7 juilet 2013
filter (df, month == 7 & origin == "JFK" & year == 2013 & day ==7)

#vols partis en juillet ou en aout
filter(df, month == 7 | month == 8)

#vols de mars à juin
filter(df, month == 3 | month ==4 |month ==5|month==6)
filter(df, month %in% c(3,4,5,6)) #identique que précédent

#ordonner par heure de depart
arrange(df,sched_dep_time)
#en sens inverse il faut mettre un moins devant
arrange(df, -sched_dep_time)

#ordonner par pls var
arrange(df, origin, month,sched_dep_time)

### SELECT -- selectionner des variables
select(df, origin, dest)

#eliminer (=drop) month et day
select(df, -month, -day)

### HELPER FUNCTIONS

#toutes var commancant ou finissant par qqch
#only arrival data
select(df, starts_with("arr"))

#only time data
select(df, ends_with("time"))

#select with "contains"
select(df, contains("t"))

###  EXCERCICE
#trouver la base de données de tous les vols partis de JFK de 4 mai 2013
#sortez la destination uniquement

#sol 1
ex <- filter(df, day == 4 & month == 5 & year == 2013 & origin == "JFK")
select(ex, dest)
#sol 2 
select(filter(df, day == 4 & month == 5 & year == 2013 & origin == "JFK"))
##sol 3 optimal : PIPE  #CTRL+MAJ+M
df %>%
  filter(day == 4 & month == 5 & year == 2013 & origin == "JFK") %>% 
  select(dest)
  
###EXERCICE POUR LA PAUSE
#lister les vols partis entre midi et 14h le 3 juillet
#montrez moijuste l'heure de départ et d'arrivée
#ordonnez les données par aeroport

df %>% 
  filter(dep_time >= 1200 & dep_time <=1400 & month == 7 & day == 3) %>% 
  arrange(origin) %>% 
  select(dep_time,arr_time)

rename(df, mois = month)

## MUTATE
#change le contenu des var, en creant de new var
#mutate creer des nouvelles var, il les stocke a la fin du DF

#normalize year to years from now
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  select(year_from_now)

# creer une var "vitesse" et une "year from now"
df %>% 
  mutate(year_from_now = 2021 - year, 
         speed = distance / air_time) %>% 
  select(year_from_now, distance, air_time, speed)
# creer une var vitesse qui soit compréhensible aux européens
df %>% 
  mutate( mph = 60*(distance /air_time), kmh = mph*1.6) %>% 
  select(kmh)

# mutate avec une fonction logique 
# var qui nous dit si l'aeroport est JFK
df %>% 
  mutate(isJFK = origin =="JFK") %>% 
  select(origin, isJFK)

## SUMMARIZE
# prend un vecteur et retourne une valeur

#retard moyen des avions
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE), #na.rm = enlever NA
            mean_delay_depart = mean(arr_delay, na.rm = T),
            sd_delay_depart = sd(dep_delay, na.rm = T))

## GROUP BY : grouper les obs
# grouper par aeroports d'orgine
df %>% 
  group_by(origin)

#plusieur var
df %>% 
  group_by(origin, month)


## combinaison de group by et summarize
# quel est le retard moyen par aeropory
df %>% 
  group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

#mean delay par mois
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

##EXERCICE
#what is the speed of flights departing around midday (11-13) by month?

df %>% 
  filter(dep_time >= 1100 & dep_time <= 1300) %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

# what is the mean delay of flights by carrier, for each month?
#quel est le retard moyen par compagnie par mois
df %>% 
  group_by(month, carrier) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

#what is the max departure delay occurred for each of the tree NYC airports, by each day?
#quel est le max de retard par jour pour chacun des 3 aeroports de NYC
df %>% 
  group_by(origin, month, day) %>% 
  summarise(maxdelay = max(dep_delay, na.rm = T))

# what is the mean air time for each of these 3 aeroports from which airport do 
#the longer haul flights depart?
df %>% 
  group_by(origin) %>% 
  summarise(mean_haul = mean(air_time, na.rm = T))

install.packages("babynames")  
# quel était le nom de fille le plus utilisé aux EU en 1947
library(babynames)
df <- babynames

df %>% 
  filter(year == 1950) %>% 
  group_by(sex) %>% 
  top_n(1)

