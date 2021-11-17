# on a télécharger le package (nycflights13)
library(nycflights13)

df <- flights

#statistiques descriptives
summary(flights)
#NA's -> données manquantes 


# exploration des données (descriptives+mini histogram) avec skimr 
# de + on voit les données manquantes 
install.packages("skimr") #pour installer le package (skimr ici)
skimr::skim(df) #pour faire apparaitre

# variable names 
names(df)

#tabulate data
table(df$origin)

#on doit toujours commencé en debut de cours par :
library(tidyverse)
#ça permet de charger les formules de tidyverse

#théorie (cours) : 
# une fonction est un verbe. Chaque fonction a son argument 
# exemple : destroy(DF,..) -> destroy est un verbe 
# il retourne toujours DF (en gros il prends des données et nous les renvoit.
# il ne touche pas aux données en mémoires. 

### filter

# one condition 
filter(df, month == 6)

#more conditions 
filter(df, month == 6 & day ==1) 

#combien de vols sont partis de JFK le 7 juillet 2013 ? 
filter(df, month ==7 & day ==7 & origin == "JFK" & year == 2013)

# vols partis en juillet OU en aout 
filter(df, month == 7 | month == 8)

# vols de mars à juin 
filter(df, month == 3 | month == 4 | month == 5 | month == 6)

#better!
filter(df, month %in% c(3,4,5,6))

### ARRANGE -> order data 

# ordonner par heure de départ 
arrange(df, sched_dep_time)

# ordonner par heure de départ en sens INVERSE
# utiliser "-" en face de la variable 
arrange(df,-sched_dep_time)

#ordonner par plusieurs varaibles
#ici d'abord par origine, puis par le mois, puis par le l'horaire de départ
arrange(df, origin, month, sched_dep_time)

##### SELEC -- selectionner des variables

# origin and destination only (marque seulement ça quand on cherche)
select(df, origin, dest)

# "drop" month and day(prends toutes les variables sans mois et day)
select(df, -month, -day)

### helper function

# all variables starting or ending with something

# only arrival data (il selectionne les variables commencant arr)
select(df, starts_with("arr"))

# only time data 
select(df, ends_with("time"))

# select with "contains"
# toutes les variables qui contiennent un "t"
select(df, contains("t"))

### exercice 

## trouver la base de données de tous les vols partis de JFK le 4 mai 2013
## sortez la destination uniquement 

filter(df, origin == "JFK" & month == 5 & day == 4)
select(filter(df, origin == "JFK" & month == 5 & day == 4), dest)


## solution 1
exo1 <- filter(df, origin == "JFK" & month == 5 & day ==4)
select(exo1, dest)

## solution 2
select(filter(df, origin == "JFK" & month == 5 & day == 4), dest)

## solution 3 : PIPE (et après) 
# la meilleure solution (CTRL+MAJ+M) 
# la PIPE prends le df, donc pas besoin de l'écrire dans filter
df %>% 
  filter(origin == "JFK" & month == 5 & day == 4) %>% 
  select(dest)

## exercice pour la pause 
#listez les vols partis entre midi et 14h le 3 juillet
# montrez moi juste l'heure de départ et d'arrivée
# ordonnez les données par aéroport 

df %>% 
  filter(dep_time >= 1200 & dep_time <=1400 & month == 7 & day ==3) %>% 
  arrange(origin) %>% 
  select(dep_time,arr_time)

# RENAME 
## le = assigne la variable et le == pose une question à la variable
df %>% 
  rename(mois = month, jour = day, année = year)

## MUTATE
## change le contenu des variables, en créant des nouvelles variables 
## avec les mêmes nombres de colonnes
## normalize year to years from now
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  select(year, year_from_now)

## créez une variable "vitesse" et une "year from now"
df %>% 
  mutate(year_from_now = 2021 -year,
         speed = distance / air_time) %>% 
  select(year_from_now, distance, air_time, speed)

## créer une variable vitesse qui soit compréhensible aux EU
df %>% 
  mutate(mph = 60*(distance / air_time),
         kmh = mph*1.6)

## mutate crée des nouvelles variables, il le stocke à la fin du DF  

## mutate avec fonction logique 
## variable qui nous dit si aéroport est JFK
df %>% 
  mutate(isJFK = origin =="JFK") %>% 
  select(origin, isJFK)

### SUMMARISE : prends un vecteur, retourne UNE valeur 

#retard moyen des avions 
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE ),
            mean_delay_arrive = mean(arr_delay, na.rm = T),
            sd_delay_depart = sd(dep_delay, na.rm = T))
# fait moi la moyenne sans tenir compte des valeurs manquantes (grâce à na.rm=TRUE)
# rm -> remouve na pour dire que c'est juste (en gros ne t'en occupe pas)
# summarise transforme en une valeur

### GROUP-BY : grouper les observations 

## groupement simple

## grouper par aéroport d'origine 
df %>% 
  group_by(origin)

## grouper par plusieurs variables

## month and origin
df %>% 
  group_by(origin,month)

## combinaison de summarize et de group_by

## quel est le retard moyen par aéroport?
df %>% 
  group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay,na.rm = T))

# mean delay by month
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay,na.rm = T))


### exercice : what is the speed of flights (moyenne des retards) departing around midday (11h-13h) by month ? 

df %>% 
  filter(dep_time >= 1100 & dep_time<= 1300) %>% 
    group_by(month) %>%
  summarise(meandelay_speed = mean(dep_delay, na.rm = TRUE ))

names(df)

## quel est le retard moyen par compagnie par mois ? 
df %>% 
  group_by(carrier, month) %>% 
  summarise(meandelay_carrier =mean(dep_delay, na.rm = TRUE))

# En plus pour pouvoir voir :
retard_m_compagnie <- df %>% 
  group_by(carrier, month) %>% 
  summarise(meandelay_carrier =mean(dep_delay, na.rm = TRUE))

view(retard_m_compagnie)
  
## Quel est le retard maximum des départs, pour chacunes des trois compagnies NYC pour chaque jour 

df %>% 
  group_by(origin, month, day) %>% 
  summarise(max(dep_delay,na.rm = T))

## rom which airport do the longer haul flights depart ? 

df %>% 
  group_by(origin) %>% 
  summarise(mean_haul = mean(air_time, na.rm = T))

#### ON A VU : filter, select, mutate, summarise and group_by

###Babynames
install.packages("babynames")

#quel était le nom de la fille le plus utilisé aux EEUU en 1947 ? 

library(babynames)
df <- babynames
df %>%
  filter(year == 1910) %>% 
  group_by(sex) %>% 
  top_n(1)

df %>%
  filter(year == 2008) %>% 
  group_by(sex) %>% 
  top_n(1)

