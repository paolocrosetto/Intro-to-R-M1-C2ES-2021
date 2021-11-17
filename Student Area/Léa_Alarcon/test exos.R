library(nycflights13)
library(tidyverse)

df <- flights

names(df)

### exercice 

## trouver la base de données de tous les vols partis de JFK le 4 mai 2013
## sortez la destination uniquement 


df %>% 
  filter(origin == "JFK" & day == 4 & month == 5) %>% 
  select(dest)
  

## exercice pour la pause 
#listez les vols partis entre midi et 14h le 3 juillet
# montrez moi juste l'heure de départ et d'arrivée
# ordonnez les données par aéroport 

df %>% 
  filter(dep_time >= 1200 & dep_time <= 1400 & day==3 & month == 7 ) %>% 
  arrange(origin) %>% 
  select(sched_arr_time, sched_dep_time) 


## créer une variable durée du voyage 

df %>% 
  mutate(duree_voyage_en_heure = (sched_arr_time - sched_dep_time)/100) %>% 
  select(flight, duree_voyage_en_heure)

# mean delay by month

df %>% 
  group_by(month) %>% 
  summarise(mean_retard_by_month = mean(dep_delay, na.rm=TRUE))

### exercice : what is the speed of flights (moyenne des retards) departing around midday (11h-13h) by month ? 
A REFAIREEEE


names(df)
## quel est le retard moyen par compagnie par mois ? 

df %>% 
  group_by(origin, month) %>% 
  summarise(retard_moyen=mean(dep_delay, na.rm=T))
  


## Quel est le retard maximum des départs, pour chacunes des trois compagnies NYC pour chaque jour 

df %>% 
  group_by(origin, )


#quel était le nom de la fille le plus utilisé aux EEUU en 1947 ? 

library(babynames)

bn <- babynames
View(bn)

bn %>% 
  filter(year == 1947) %>% 
  group_by(sex) %>% 
  top_n(1)
  
  
bn %>% 
  filter(year == 1947 & sex == "F") %>% 
  top_n(10)
  
sd(9)
  
ggplot(df)
  