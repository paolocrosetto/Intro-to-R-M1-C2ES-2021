#import into our library the package nycflights
#
#differents possibilitys to see the package :
library(nycflights13)
#visualise the category flights in nycflights
flights
#instantiation in df of flights
df <- flights 

#descriptive statistics
summary(df)

#exploring the data with skimr
install.packages("skimr")
#other possibility to see data's
skimr::skim(df)

#all the script will start with
library(tidyverse)

#Structure of dplyr : all functions is à direct verbs
#filter by the month for all the flight of june
filter(df, month == 6)

#more conditions
filter(df, month == 6 & day == 1) #june 1st flights

# combien de vols sont partis de jfk le 7 juillet 2013?
filter(df, month == 7 & day == 7 & origin == "JFK" & year == 2013)

#vols partis en juillet ou en aout
filter(df, month == 7 | month == 8)
filter(df, month == 7 | month == 8 | month == 5)

#better 
filter(df, month %in% c(3,4,5,6))

# Arrange order data by :
#order by departure time (min to max) 
arrange(df,sched_dep_time)

#order in reverse way
arrange(df, -sched_dep_time)

#order by differents variables
arrange(df, origin,month, sched_dep_time)
#order by column, if you won't a variable insert '-' in front of the variable

select(df, origin, dest)

# "drop" month and day
select(df, -month, -day)

#names of the variables
names(df)
#all variables starting or ending with something
#start by time with
select(df, starts_with("arr"))
#only time data
select(df,ends_with("time"))

#select with contains
#toutes les var qui contiennent un t
select(df,contains("t"))

###exercice

#trouver la bd de tout les vols partis de jfk le 4 mai 2013
#sortir la destination uniquement
dfs <- filter(df, month == 5 & day == 4 & origin == "JFK" & year == 2013)
select(dfs,dest)
#Better solution : PIPE
# %>% signifie et après CTRL+MAJ+M
 df %>% 
   filter(month == 5 & day == 4 & origin == "JFK" & year == 2013) %>% 
   select(dest)

 
 #Exercice pour la pause 
 
 #Listez les vols partis entre midi et 14h
 
df %>% 
  filter(dep_time %in% c(1200:1400) & month == 7 & day == 3) %>%
  arrange(origin) %>%  
  select(dep_time,arr_time)

#renommer les données 
# rename(df, mois = month) -> rename(data, newname = oldname)

#Rename
df %>% 
  rename (mois = month, jour = day, année = year , onesttropforts = dep_time)

#Conditions : == condition logique, = instaure une égalité

#Creation d'une nouvelle variable en appliquant une transformation à la data : mutate()
#mutate(data, newvar = f(oldvar))


#normalize year to years from now
df %>%  
  mutate(year_from_now = 2021 - year) %>% 
  select(year_from_now)

#creer une variable vitesse et une year from now
df %>% 
  mutate(year_from_now = 2021 - year) %>% 
  mutate(speed = distance / air_time) %>% 
  select(year_from_now, distance , air_time,speed)

#creer la vitesse 
df %>% 
  mutate(mph = distance / (air_time*60/1.6), kmh = mph*1.6) %>% 
  select(mph, kmh)
#On peut effectuer plusieurs créations de variables avec mutate 

df %>% 
  mutate(isJFK = origin == "JFK") %>% 
  select(origin, isJFK)

#Summarise : prend un vecteur retourne une valeur
#retard moyen des avions 
df %>%  
  summarise(mean_delay_depart = mean(dep_delay,na.rm = TRUE),
            mean_delay_arrive = mean(arr_delay,na.rm = T),
            sd_delay_depart = sd(dep_delay, na.rm = T))
#na.rm= TRUE ou T ne prend pas en compte les valeurs manquantes 

#sd -> ecart type

#Group By : grouper les observations

#groupement simple
##grouper par aeroport d'origine
df %>%  
  group_by(origin)

##Grouper par plusieurs variables
##month and origins
df %>% 
  group_by(origin,month)

##Combinaison de summarize et de groupe by

##Quel est le retard moyen par aeroport ?
df %>% 
  group_by(origin) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))


#Filter Select Mutate, Summarize and Group_by
df %>% 
  group_by(month) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))

#Exercice Quelle est la vitesse moyenne des vols partants entre 11H et 13h par mois
#filter group By summarise 
df %>% 
  filter(dep_time %in% c(1100:1300)) %>% 
  mutate(speed = distance / air_time*60*1.6 ) %>% 
  group_by(month) %>% 
  summarise(meanspeed = mean(speed, na.rm = T))


#1.What is the mean delay of flights by carrier, for each month ?
#2.What is the maximum departure delay occured for each of the three NYC airports, by each day
#3.What is the mean air time for eache of these three airports
#from which airport do the longer haul flights
#1
df %>%  
  group_by(month,carrier) %>% 
  summarise(meandelay = mean(dep_delay, na.rm = T))
#2
df %>% 
  group_by(day,origin) %>% 
  summarise(max(dep_delay,na.rm = T))
#3
df %>% 
  group_by(origin) %>% 
  summarise(mean(air_time, na.rm = T))

#Internation : JFK, Local : LGA, interregional : EWR


install.packages("babynames")

library(babynames)

ls2 <- babynames 

ls2 %>% filter(year == 1945) %>% 
  group_by(name) %>% 
  top_n(10)
