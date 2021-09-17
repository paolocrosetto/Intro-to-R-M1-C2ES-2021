                    #II Data import,export etc
library(tidyverse)
library(nycflights13)
# import data 
df <- flights
View(df)
#Stats desc
summary(df)
skimr::skim(df) #Plus visuel que summary
#nom des variables
names(df)
table(df$origin)


  #TRIER DES DONNEES

#Filter
filter(df, month)
#cb vols partis de JFK le 7 juillet 2013?
filter(df, df$month == 7 | df$month==8)
# %in% pour une plage
filter(df, month %in% c(3,4,5,6))

###ARRANGE (trie les données)
#ordonner par ordre de départ
arrange(df, sched_dep_time)
arrange(df, -sched_dep_time) #pour le sens inverse
arrange(df,origin,month, sched_dep_time) #tri sur origine puis months puis heure de depart

    #SELECTIONNER DES VARIABLE

#select pour selectionner les variables
select(df, origin,dest)
#Select columns: here, only arrival data
select(df,starts_with("arr"))

#select with contains: toutes les variables qui contiennent un "t"
select(df,contains("t"))


#exercice
#Trouver la bdd de tous les vols partis de JFK le 4 mai 2013
bddjfk<-filter(df,month==5,day==4,year==2013,origin=="JFK")
#Sortez la destination uniquement
select(bddjfk,dest)

#SOLUTION --> Les PIPE 
#ctrl maj m 
#pour obtenir %>%
df %>% 
  filter(month==5,day==4,year==2013,origin=="JFK") %>% 
  select(dest)

#vols entre midi et 14h le 3 juillet
#montrer juste l'heure de départ et d'arrivée
#ordonner les données par aéroport

df %>% 
  filter(month==7,day==3,hour %in% c(12:13)) %>% 
  arrange(origin) %>% 
  select(sched_dep_time,sched_arr_time)


    #MODIFIER DES VARIABLES
#rename variables: rename()
rename(df,mois=month)
  
#MUTATE

#change le contenu des variabels en creant des nouvelles variables
#normalize year to years from now
df %>% 
  mutate(year_from_now=2021 - year) %>% 
  select(year,year_from_now)


#Creer une variable "vitesse" et une "year from now"
df %>% 
  mutate(year_from_now=2021 - year, speed = distance/air_time) %>% 
  select(year_from_now,distance,air_time,speed)

#mutate avec des fonctions logiques
df %>% 
  mutate(isJFK=origin == "JFK") %>% 
  select(origin, isJFK)

#SUMMARIZE renvoie une seule valeur a partir de nombreuses données
# /!\ Modifie les données
#retard moyen
df %>% 
  summarise(mean_delay_depart = mean(dep_delay, na.rm=T),
            mean_delay_arrive = mean(arr_delay, na.rm=T))


##GROUP BY : grouper les observations
df %>% 
  group_by(origin)

## month and origin
df %>% 
  group_by(origin, month)

#On peut combiner summarize et group_by
#Quel est le retard moyen par aeroport?
  df %>% 
    group_by(origin) %>% 
    summarise(meandelay = mean(dep_delay,na.rm=T))
  
  # mean delay by month
  df %>% 
    group_by(month) %>% 
    summarise(meandelay = mean(dep_delay,na.rm=T))
  
  
  
  #What is the average delay of departing around midday (11-13), by month
  df %>%
    filter(hour %in% c(11:12)) %>%
    group_by(carrier,month) %>% 
    summarise(mean(dep_delay,na.rm=T))
  #OU
  df %>% 
    filter(dep_time>=1100 & dep_time <=1300) %>%
    group_by(carrier,month) %>% 
    summarise(mean(dep_delay,na.rm=T))

  #What is the maximum departure delay occured for each of the three NYC airports, by each day?
  df %>% 
    filter(dep_time>=1100 & dep_time <=1300) %>%
    group_by(carrier,month) %>% 
    summarise(max(dep_delay,na.rm=T))
  
  #What is the mean air time for each of these three airports?
  #from which aiports do the longer haul flights depart?

    table(df$carrier)
  
  