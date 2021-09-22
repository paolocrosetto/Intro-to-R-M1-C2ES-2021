
### Cours 2 - 17/09/2021


# charge toutes les fonctions qu'on va utiliser par la suite : il faut le mettre
# à chaque début de cours
library(tidyverse)

# montre les données airlines dans le package nycflight13
nycflights13::airlines 

# prépare le package à être utilisé
library(nycflights13) 

# importe les données dans notre espace
df <- flights

# ouvre le tableau de df : on regarde une première fois les données
view(df)


### Informations sur les données

# statistiques descriptives
summary(df)

# exploration des données avec skimr
install.packages("skimr")

# donne aussi des statistiques descriptives mais en plus détaillé et avec 
# histogramme --> regarder si il manque des données
skimr::skim(df)


### filter 

# renvoie les lignes de df pour lesquelles le mois est 6
filter(df, month == 6)

# plusieurs conditions
filter (df, month == 6 & day == 1) # premier vol du mois de juin

# tabulation --> donne les valeurs possibles prises par la variable et le nombre
# d'apparition de chaque valeur
table(df$origin)

# combien de vols sont partis de JFK le 7 juillet 2013 ?
filter(df, month == 7 & day ==7 & origin == "JFK" & year == 2013)

# vols partis en juillet ou en août
filter(df, month == 7 | month == 8)

# vols de mars à juin
filter(df, month %in% c(3,4,5,6))


### arrange -> order data

# ordonner par heure de départ
arrange(df, sched_dep_time)

# ordonner par heure de départ en sens inverse
arrange(df, -sched_dep_time)


### select 

# seulement origine et destination
select(df, origin, dest)

# enlever des variables
select(df, -month, -day)


### helper functions

# toutes les variables qui commencent ou finissent par quelques chose 
select(df, starts_with("arr"))

# only time data
select(df, ends_with("time"))

# select with contains
# toutes les variables qui contiennent un "t"
select(df, contains("t"))


### exercices

## trouver la base de données de tous les vols partis de JFK le 4 mai 2013
## sortez la destination uniquement

df2 <- filter(df, origin == "JFK" & day == 4 & month == 5)
select(df2, dest)

# meilleure solution : PIPE (CTRR+MAJ+M) -> prend le résultat de l'opération d'avant et le
# met comme premier argument de la fonction d'après

df %>% 
  filter(origin == "JFK" & day == 4 & month == 5) %>% 
  select(dest)

## listez les vols partis entre midi et 14H le 3 juillet
## montrer juste l'heure de départ et d'arrivée
## ordonner les données par aéroport

df %>% 
  filter(dep_time %in% c(1200:1400) & day == 3 & month == 7) %>%
  arrange(origin) %>% 
  select(dep_time, arr_time)
  

### rename
# version de select qui garde toutes les variables mais les renome

df %>%
  rename(mois = month, jour = day) # renomme "mois" la variable "month" 
# et "jour" la variable "day" mais sans stocker le nouveau tableau

df %>%
  rename(mois = month, jour = day)


### mutate
# change le contenu des variables, en créant des nouvelles variables

# crée une nouvelle variable au tableau qui prend la valeur 2021-year
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  select(year, year_from_now)

# crée une variable "vitesse" et une "year from now"
df %>% 
  mutate(year_from_now = 2021 - year, 
         speed = distance / air_time) %>% 
  select(year_from_now, distance, air_time, speed)


# crée une variable "vitesse" qui soit compréhensible aux EU
df %>% 
  mutate(speed = distance / air_time * 60,
         readable_speed = speed*1.6) %>% 
  select(ends_with("eed"))

# mutate avec fonction logique

df %>% 
  mutate(lsJFK = origin == "JFK") %>% 
  select(origin, lsJFK)


### lag : to refer to the -1 period


### Summarise : prend un vecteur, retourne une valeur

df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE), 
            mean_delay_arrive = mean(arr_delay, na.rm = T),
            sd_delay_depart = sd(dep_delay, na.rm = T))
# na.rm = T permet de dire de calculer la moyenne même s'il y a des valeurs
# manquantes


### Group by : grouper les observations

# groupement simple

# groupe par aéroport d'origine
df %>% 
  group_by(origin)

# grouper par plusieurs variables

# mois et origines
df %>% 
  group_by(origin, month)

# combinaison de summarize et de group_by

# quel est le retard moyen par aéroport ? 
df %>% 
  group_by(origin) %>%
  summarise(meandelay = mean(dep_delay, na.rm = T))

# mean delay by month
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

# quelle est la vitesse des vols qui partent de 11 à 13h par mois ?
df %>%
  filter(dep_time %in% (1100:1300)) %>%
  group_by(month) %>%
  mutate(vitesse = distance/air_time) %>%
  summarise(meandelay= mean(vitesse, na.rm =T ))
# voir correction

### Exercices

## 1 : retard moyen par compagnie pour chaque mois
df %>% 
  group_by(carrier,month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))


## 2 : plus gros retard pour chaque aéroport pour chaque jour
df %>% 
  group_by(origin, day, month) %>% 
  summarise(maxdelay = max(dep_delay, na.rm = T))


## 3 : temps de vol moyen pour chaque aéroport ? Pour quel aéroport le temps 
##     est-il le plus long ?
df %>% 
  group_by(origin) %>% 
  summarise(meantime = mean(air_time, na.rm = T))
  
## 4 : quel était le nom de fille le plus utilisé aux EU en 1947
install.packages("babynames")
library("babynames")
df <- babynames

df2 <- df %>% 
  filter(year == 1947, sex == "F") %>% 
  arrange(-n) %>% 
  select(name)

df2[1,]





  

















































