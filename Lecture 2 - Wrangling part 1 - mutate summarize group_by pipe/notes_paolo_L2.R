
# attach package to working area
library(nycflights13)

# import data in our space
df <- flights


### get information on data

# statistiques descriptives
summary(flights)

# exploration des données avec skimr
install.packages("skimr")

# skimming
skimr::skim(df)

# variable names
names(df)

# tabulate data
table(df$origin)


library(tidyverse)


### filter

# one condition
filter(df, month == 6) ## june flights

# more conditions
filter(df, month == 6 & day == 1) ## june 1st flights

# combien de vols sont partis de JFK le 7 juillet 2013?
filter(df, month == 7 & day == 7 & origin == "JFK" & year == 2013)

# vols partis en juillet OU en aout
filter(df, month == 7 | month == 8)

# vols de mars à juin
filter(df, month == 3 | month == 4 | month == 5 | month == 6)

# better!
filter(df, month %in% c(3,4,5,6))

### ARRANGE -> order data

# ordonner par heure de départ
arrange(df, sched_dep_time)

# ordonner par heure de départ en sens INVERSE
# utilisez "-" en face de la variable
arrange(df, -sched_dep_time)

#ordonner par plusieurs variables
arrange(df, origin, month, sched_dep_time)

#### SELECT -- selectionner des variables

# origin and destination only
select(df, origin, dest)

# "drop" month and day
select(df, -month, -day)



### helper functions

# all variables starting or ending with something


# only arrival data
select(df, starts_with("arr"))

# only time data
select(df, ends_with("time"))

# select with "contains"
# toutes les variables qui continnent un "t"
select(df, contains("t"))


### exercice

## trouvez la base de données de tous les vols partis de JFK le 4 mai 2013
## sortez la destination uniquement

## solution 1
exo1 <- filter(df, origin == "JFK" & month == 5 & day== 4)
select(exo1, dest)

## solution 2
select(filter(df, origin == "JFK" & month == 5 & day== 4), dest)

## solution 3: PIPE
df %>% 
  filter(origin == "JFK" & month == 5 & day== 4) %>% 
  select(dest)

# PIPE : CTRL+MAJ+M


## exerccie pour la pause

# listez les vols partis entre midi et 14h le 3 juillet
# montrez moi juste l'heure de départ et d'arrivée
# ordonnez les données par aéroport
df %>% 
  filter(dep_time >= 1200 & dep_time <=1400 & month == 7 & day == 3) %>% 
  arrange(origin) %>% 
  select(dep_time, arr_time)

# RENAME
df %>% 
  rename(mois = month, jour = day, année = year)

## MUTATE
## change le contenu des variables, en créant des nouvelles variables

## normalize year to years from now
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  select(year, year_from_now)

## créez une variable "vitesse" et une "year from now"
df %>% 
  mutate(year_from_now = 2021 - year,
         speed = distance / air_time) %>% 
  select(year_from_now, distance, air_time, speed)

## créez une vairbale "vitesse" qui soit compréhensible aux EU
df %>% 
  mutate(mph = 60*(distance / air_time),
         kmh = mph*1.6)

## mutate crée des nouvelles variables, il les stocke à la fin du DF

## mutate avec fonction logique
## variable qui nous dit si aéroport est JFK
df %>% 
  mutate(isJFK = origin == "JFK") %>% 
  select(origin, isJFK)
  

### SUMMARISE: perend un vecteur, retourne UNE valeur

# retard moyen des avions
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE),
            mean_delay_arrive = mean(arr_delay, na.rm = T),
            sd_delay_depart = sd(dep_delay, na.rm = T))


### GROUP-BY : grouper les observations

## groupement simple

## grouper par aéroport d'origine
df %>% 
  group_by(origin)

## grouper par plusieurs variables

## month and origin
df %>% 
  group_by(origin, month)


## Combinaison de summarize et de group_by

## quel est le retard moyen par aéroport?
df %>% 
  group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

# mean delay by month
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))


## exercice: vitesse moyenne des vols au départ entre 11h et 13h par mois
df %>% 
  filter(dep_time >= 1100 & dep_time <= 1300) %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

## what is the mean delay of flights by carrier, for each month?
df %>% 
  group_by(carrier, month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

## what is the maximum departure delay occurred for
## each of the three NYC airports, by each day?
df %>% 
  group_by(origin, month, day) %>% 
  summarise(maxdelay = max(dep_delay, na.rm = T))


##what is the mean air time for each of these three airports? 
## rom which airport do the longer haul flights depart?
df %>% 
  group_by(origin) %>% 
  summarise(mean_haul = mean(air_time, na.rm = T))

## babynames!!!
install.packaes("babynames")
library(babynames)
df <- babynames

df %>% 
  filter(year == 2017) %>% 
  group_by(sex) %>% 
  top_n(1)
