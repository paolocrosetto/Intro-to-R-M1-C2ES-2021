install.packages("nycflights13")

nycflights13::flights

# attach package to working area
library(nycflights13)

# importe data in your space
df <- flights
View(flights)

### Avoir des informations sur les données

# statistiques descriptives
summary(flights)

# exploration des données avec skimr
install.packages("skimr")
skimr::skim(df)

install.packages("tidyverse")
library(tidyverse)

### filter
filter(df, month == 6) ## june flights

#more condition
filter(df, month == 6 & day == 1) ## june 1st flights

# combien de vols sont partis de JFK le 7 juillet 2013 ?
filter(df, month == 7 & day == 7 & origin == "JFK" & year == 2013)

# vols partis en juillet ou en aout
filter(df, month == 7 | month == 8)

# vols de mars à juin
filter(df, month == 3 | month == 4 | month == 5 | month == 6) ## fonctionne mais pas optimisé
filter(df, month %in% c(3,4,5,6))

### Arranger des données -> order data

# ordonner par heure de départ
arrange(df, sched_dep_time)

# ordonner par heure de départ en sens inverse
arrange(df, -sched_dep_time)            # utiliser un "-" devant la variable

# ordonner avec plusieurs variables
arrange(df, origin, month, sched_dep_time)

### Selectionner des variables

# origin et destination seulement
select(df, origin, dest)

# "drop" onth and day
select(df, -month, -day)                # on illimine deux variables

### Helper functions

# toutes les variables qui commencent et qui finissent avec une donnée

# seulement les données qui commencent par "arr"
select(df, starts_with("arr"))

# seulement les données se terminant par "time"
select(df, ends_with("time"))

# selection avec contains
# toutes les variables qui contiennent "t"
select(df, contains("t"))

### Exercice 1

## Trouver la base de données de tous les vols partis de JFK le 4 mai 2013
data <- filter(df, origin == "JFK" & year == "2013" & month == "5" & day == "4")
## Sortez la destination uniquement
select(data, dest)

# Solution 1 : (same)

# Solution 2 :
select(filter(df, origin == "JFK" & year == "2013" & month == "5" & day == "4"), dest)

# Solution 3 : PIPE (CTRL+MAJ+M : %>% ) -> on ne créer pas d'objets inutile, pas de filtre
df %>% 
  filter(origin == "JFK" & year == "2013" & month == "5" & day == "4") %>% 
  select(dest)

### Exercice 2

## Listez les vols partis entre 12h et 14h le 3 juillet
## Montrez moi juste l'heure de départ et d'arrivée
## Ordonnez les données par aéroport

# Solution :
df %>% 
  filter(day == "3" & month == "7" & dep_time >= 1200 & dep_time <= 1400) %>% 
  arrange(origin) %>% 
  select(dep_time, arr_time)



## On peut renommer des variables avec "rename()" 
# Exemple : rename(data, newname = oldname)

# Rename
df %>% 
  rename(mois = month, jour = day, annee = year)

## On peut changer le contenu des variables en créant de nouvelles variables avec "mutate()"
# Normalize year to years from now
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  select(year, year_from_now)

# Création d'une variable vitesse et une "year from now"
df %>% 
  mutate(year_from_now = 2021 - year,
         speed = distance / air_time) %>% 
  select(year_from_now, distance, air_time, spedd)
         
# Création d'une variable "vitesse" qui soit compréhensible aux EU
df %>% 
  mutate(mph = 60*(distance / air_time),kmh = mph*1,6)

## Mutate créer des nouvelles variables, il les stocke à la fin du df

# Variable qui nous dit si l'aéroport est JFK
df %>% 
  mutate(isJFK = origin == "JFK") %>% 
  select(origin, isJFK)

## On peut prendre un vecteur qui nous retournera une valeur avec "Summarise()"

# Retard moyens des avions
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE),
            mean_delay_arrive = mean(arr_delay, na.rm = TRUE),
            sd_delay_depart = sd(dep_delay, na.rm = TRUE))

## On peut grouper les données avec "group-by()"

# Groupement simple
df %>% 
  group_by(origin, month)

## Combinaison de summarise() et de group_by()
# Quel est le retard moyen par aéroport ? 
df %>% 
  group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = TRUE))

# Mean delay by month
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = TRUE))


### Exercice 3

## Trouver la vitesse moyenne des vols en départ autour de midi (11-13) par mois ? 
df %>% 
  filter(dep_time >= 1100 & dep_time <= 1300) %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = TRUE))



#### Exercices de fin de cours
### Exercice 4

## Quelle est la moyenne de retard d'avion par compagnie pour chaque mois ?
df %>% 
  group_by(month, carrier) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = TRUE))

### Exercice 5

## Quel est le maximum de retard de départ pour chacun des 3 aéroport pour chaque jour ?
df %>% 
  group_by(month, day, origin) %>% 
  summarise(maxdelay = max(dep_delay, na.rm = TRUE))

### Exercice 6

## Quelle est la moyenne de temps pour chacun des 3 aéroport ? Pour chaque aéroport donner 
df %>% 
  group_by(origin) %>% 
  summarise(mean_haul = mean(air_time, na.rm = TRUE))


### Petit exo supplémentaire

install.packages("babynames")
library(babynames)
df <- babynames
df %>% 
  filter(year == 2000) %>% 
  group_by(sex) %>% 
  top_n(1)
