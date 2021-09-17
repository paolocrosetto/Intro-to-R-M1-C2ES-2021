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
