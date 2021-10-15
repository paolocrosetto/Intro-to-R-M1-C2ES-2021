install.packages("nycflights13")
library(nycflights13)
df <-flights #on a importé une base de données
View(flights)

#statistiques descriptives
summary(flights) 
install.packages("skimr")
skimr::skim(df)
names(df)
table(df$origin)
library(tidyverse)

#filter() prend un jeu de données et crée un jeu de données avec moins de lignes
df #3336776 observations
filter(df, month == 6) #28243 observations
filter(df, month == 6 & day == 1) #754 observations
#combien de vols sont partis de JFK le 7 juillet 2013?
filter(df, year == 2013 & month == 7 & day == 7 & origin == "JFK") #327
#combien de vols sont partis en juillet ou en aout?
filter(df, month == 7 | month == 8) #58752
#combien de vols entre mars et juin?
filter(df, month == 3 | month == 4 | month == 5 | month == 6)
filter(df, month %in% c(3,4,5,6)) #114203

#arrange() pour ordonner
#ordonner par heure de départ (croissant)
arrange(df, sched_dep_time)
#ordonner par heure de départ (décroissant)
arrange(df, - sched_dep_time)
#ordonner par origine, mois et heure de départ
arrange(df, origin, month, sched_dep_time)

#select()  prend un jeu de données et crée un jeu de données avec moins de colonnes
#selectionner uniquement l'aéroport de départ et d'arrivée
select(df, origin, dest) #2 colonnes
#selectionner tout sauf le jour et le mois
select(df, -day, -month) #17 colonnes
#selectionner uniquement les données d'arrivée
select(df, starts_with("arr")) #2 colonnes
#selectionner uniquement les données relatives au temps
select(df, ends_with("time")) #5 colonnes
#selectionner toutes les variables qui contiennent un "t" dans leur nom
select(df, contains("t")) #12 colonnes

#trouver la base de données de tous les vols partis de JFK le 4/5/2013 puis sortir la destination uniquement
bd <- filter(df, origin == "JFK" & day == 4 & month == 5 & year == 2013)
select(bd, dest)

select(filter(df, origin == "JFK" & day == 4 & month == 5 & year == 2013), dest)

#solution PIPE %>% (CTRL + MAJ + M) met la base de données selectionée en premier argument de la prochaine action
df %>%
  filter(origin == "JFK" & day == 4 & month == 5 & year == 2013) %>% 
    select(dest)

#lister les vols partits entre 12h et 14h le 3/7 et montrer l'heure de départ et d'arrivée ordonné par aéroport
df %>% 
  filter(day == 3 & month == 7 & dep_time %in% c(1200:1400)) %>%
  arrange(origin) %>%
  select(dep_time,arr_time)

#rename() pour renommer
rename(df, mois = month, jour = day)
df %>% 
  rename(mois = month, jour = day, année = year)

#mutate() change le contenue des variables en créant des nouvelles variables
#changer l'année pour qu'elle indique il y a combien d'années les vols ont eu lieu
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  select(year, year_from_now)
#créer une variable vitesse et year_from_now
df %>% 
  mutate(year_from_now = 2021 - year, speed = distance / air_time) %>% 
  select(year_from_now, distance, air_time, speed)
#créer une variable vitesse qui soit compréhensible pour nous (km/h)
df %>% 
  mutate(speed = distance / air_time*60, redable_speed = speed * 1.6) %>% 
  select(ends_with("eed"))
#variable qui nous dit si l'aéroport est JFK
df %>% 
  mutate(isJFK = origin == "JFK") %>% 
  select(origin, isJFK)

#summarise() prend un vesteur et retourne une valeur
#retard moyen des avions
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE),  
            mean_delay_arrive = mean(arr_delay, na.rm = T)) # on force R à calculer même s'il manque des données => on élimine les valeurs manquantes

#group_by() permet de grouper les observations
#grouper par aéroport d'origine
df %>% 
  group_by(origin)
#grouper par aéroport d'origine et par mois
df %>% 
  group_by(origin, month)

#summarise() et group_by()
#quel est le retard moyen par aéroport?
df %>% 
  group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay, na.rm =T))
#quel est le retard moyen par mois?
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm =T))

#quelle est la vitesse moyenne des vols qui partent entre 11h et 13h par mois?
#en km/h
df %>% 
  filter(dep_time %in% c(1100:1300)) %>% 
  mutate(speed = distance/air_time*60*1.6) %>% 
  group_by(month) %>% 
  summarise(meanspeed = mean(speed, na.rm =T))
#en miles/minutes
df %>% 
  filter(dep_time >= 1100 & dep_time <= 1300) %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))
  
#retard moyen par companie par mois
df %>% 
  group_by(carrier, month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

#retard maximum pour chacun des aéroports par jour
df %>% 
  group_by(origin, day, month) %>% 
  summarise(maxdelay = max(dep_delay, na.rm = T))

#temps de vol moyen de chacun des aéroports
df %>% 
  group_by(origin) %>% 
  summarise(meanairtime = mean(air_time, na.rm=T))
#JFK fait les plus long temps de vols => aéroport international

install.packages("babynames")
#quel était le nom de fille le plus utilisé aux EU en 1947?
library(babynames)
bb <- babynames
bb %>% 
  filter(sex=='F' & year==1947) %>% 
  top_n(5) # donne le top 5
#le nom de la fille et du garçon les plus donnés en 1910
bb %>% 
  filter(year==1910) %>% 
  group_by(sex) %>% 
  top_n(1)
  