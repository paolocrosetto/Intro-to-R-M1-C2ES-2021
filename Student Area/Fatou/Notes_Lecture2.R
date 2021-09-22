#attach package to working space
library(nycflights13)

 
#import  data in our space
df<-flights

#statistiques descriptives
summary(flights)

#exploration des donness avec skimr
install.packages("skimr")

#skimming
skimr:skim(df)

library(tidyverse)

###filter
#one condition

filter(df, month == 6) #june flights

#more conditions
filter(df, month == 6 & day == 1)#june 1st flights

#combien de vols sont partis de JFK le 7 juillet 2013
filter(df, month == 7, day == 7 & origin == "JFK" & year == 2013)

#vols partis en juillet ou en aout 
filter(df, month == 7 | month == 8)

#vols de mars à juin 
filter(df, month == 3 | month == 4 | month == 5 | month == 6)

#better 
filter(df, month %in% c(3,4,5,6) )

###ARRANGE -> order data

#ordonner par heure de départ 
#utilisez "-" en face de la variable 
 arrange(df, -sched_dep_time)

#ordonner par plusieurs variables 
 arrange(df, origin, month, sched_dep_time)

 
### SELECT -- selectionner des variables 
 
 #origin and destination only
select(df, origin, dest)

#"drop" month and day 
select(df, -month, -day)


### helper functions

#all variables starting or ending with something 
names(df)

#only arrival data
select(df, starts_with("arr"))

#only time data 
select(df, ends_with("time"))

#select with "contains"
#toutes les variables qui contiennent un t 
select(df, contains("t"))

### exercice
##trouvez la base de données de tous les vols partis de JFK le 4 mai 2013
#sortez la destination uniquement 

#solution 1 (pas optimale)
sol1<- filter(df, origin == "JFK" & month == 5 & day == 4)

select(sol1, dest)

#solution 2 (pas optimale)
select(filter(df, origin == "JFK" & month == 5 & day == 4), dest)

#meilleure solution : PIPE 
 # %>% control + maj + m
df %>% 
  filter(df, origin == "JFK" & month == 5 & day == 4) %>% 
  select(dest)

 
#exercice pour la pause
#listez les vols parits entre midi et 14h le 3 juillet
#montres juste l'heure de depart et d'arrivée 
#ordonnez les données par aeroport

df %>% 
  filter(dep_time >= 1200 & dep_time <=1400 & month == 7 & day ==3) %>% 
  arrange(origin) %>% 
  select(dep_time, arr_time)


### RENAME
df %>% rename(mois = month, jour = day, année = year)

### MUTATE
## change le contenu des variables en créant des nouvelles variables 

## normalize year to years form now
df %>% 
  mutate(year_from_now = 2021 - year )
  select(year, year_from_now)


##créez une variable "vitesse" et une "year from now"
df %>% 
  mutate(year_from_now = 2021 - year,
        speed = distance / air_time) %>%
  select(year_from_now, distance, air_time, speed)

##créez une variable "vitesse" qui soit compréhensible aux EU
df %>% 
  mutate(mph = 60*(distance / air_time),
         kph = mph*1.6) %>% 
  select(mph,kph)

##mutate crée des nouvelles variables ,{} les stocke à la fin 
#mutate avec fonction logique
#variable qui nous  dit si aeroport est JFK
df %>% 
  mutate(isJFK = origin == "JFK") %>% 
  select(origin, isJFK)

### SUMMARIZE : prend un vecteur, retourne une valeur 

# retard moyen des avions
df %>% 
  summarize(mean_delay_depart = mean(dep_delay, na.rm = TRUE),
            mean_delay_arrive = mean(arr_delay, na.rm = T))
#na c'est valeur manquante et rm remove


### GROUP-BY : grouper les observations
##groupement simple
## grouper par aeroport d'origien
df %>% 
  group_by(origin)


## Combinaison de summarize et de group_by
##quel est le retard moyen par aeroport 
df %>% 
  group_by(origin) %>% 
  summarize(mean_delay = mean(dep_delay, na.rm = T))

#exercice diapo 34
#quelle est la vitesse moyenne des departs entre 11h et 13h par mois?
df %>% 
  filter(dep_time <= 1100  & dep_time >=1300)
  group_by(month) %>% 
  summarize(mean_delay = mean(dep_delay, na.rm = T))

  