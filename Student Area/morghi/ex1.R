
# attach package to working area 
library(nycflights13)

#import data in our space 
df <- flights
# statistiques descriptive 
summary(flights)

#exploration des données avec skimr
install.packages("skimr")

skimr::skim(df)

#
install.packages("tidyverse")
library(tidyverse)

#filtrer 
# one condition
filter(df,month == 6)
#more conditions
filter(df,month == 6 & day == 1) #june 1st flights
 
#les noms des variables
names(df)

#tabulate date 
table(df$origin)

#combien de vols sont partis des jfk le 7 juillet 2013?

filter(df, month == 7 & day ==7 & origin == "JFK" & year == 2013)

#vols partis en juillet ou en aout 

filter(df, month == 8 | month ==7)

#vols de mars à juin 

filter(df,month ==3 | month ==4 |month ==5 | month ==6)

#better 
filter(df,month %in% c(3,4,5,6))

### ARRANGE -> ordre data

#ordonner par heure de départ
arrange(df,sched_dep_time)
#ordonner par heure de depart en sens d'inverse 
arrange(df,-sched_dep_time)
#ordonner par plusieurs variables 
arrange(df,origin,month,sched_dep_time)

#### selection des variables 

#origin and destination only
select(df,origin,dest)

#drop month and day
select(df,-month,-day)

###helper functions

#all variables starting or ending with something 

#only arrival data
select (df,starts_with("arr"))
#only time data
select(df,ends_with("time"))

#select with contains
#toutes les variables qui continnent un "t"
select(df,contains("t"))

### exercice 

#trouver la base de données de tous les vols partis de jfk le 4 mai 2013
#sortez la destination uniquement 
#solution 1
ex <- filter(df,origin == "JFK" & month == 5 & day == 4 & year == 2013)
ex
select(ex,dest)

#solution 2
select(filter(df,origin == "JFK" & month == 5 & day == 4 & year == 2013),dest)

#solution 3 : pipe
df %>%
   filter(origin == "JFK" & month == 5 & day == 4 & year == 2013) %>%
   select(dest)
# pipe : CRTL+MAJ+M

##EXERCICE POUR LA PAUSE MDR

#LISTEZ les vols partis entre midi et 14h le 3 juillet
#montrez moi juste l'heure de départ et d'arrivée 
#ordonnez les données par &eroport

df %>%
  filter(month == 7 & day == 3 & dep_time <= 1400 & dep_time >=1200) %>%
  arrange(origin) %>%
  select(dep_time,arr_time)

##renomer 
 df %>%
  rename(mois = month,jour= day , année = year)

 ##mutate 
 #change le contenu des variables ,en creant des nouvelles variables
 
 #normalize year to years from now 
  df %>%
    mutate(year_fom_now = 2021 - year ) %>%
    select(year,year_fom_now)

## creer une variable "vitesse" et une "year from now" 
  df %>%
    mutate(year_from_now = 2021 - year,
           speed = distance / air_time) %>%
    select(year_from_now,distance,air_time,speed)

##creer une variable "vitesse" qui soit comprehensible aux EU 
  df %>% 
    mutate(mph = 60*(distance /air_time),
           kmh= mph*1.6) %>%
    select(mph,kmh)
#mutate creer des nouvelles variables, il les stocker a la fin du DF
  
  ##mutate avec fonctions logique
  #variable qui nous dit si l'aeropor est jfk
  df %>%
    mutate(isJFK = origin == "JFK") %>%
    select(origin,isJFK)

  #summarise prend un vecteur et retourne une seule valeur
  #retard moyen des avions
   df %>%
     summarise(mean_delay_depart = mean(dep_delay,na.rm =TRUE),
               mean_delay_arrive = mean(arr_delay,na.rm =TRUE),
               sd_delay_depart = sd(dep_delay,na.rm =TRUE))

##Group by :groupe les observations 
   #exemple simple
    df %>%
      group_by(origin)
    #grouper par plusieurs variables
    #month et origin
    df %>%
      group_by(origin,month)
#combinaison de summarize et group by 
    ##quel est le retard moyen par aeroport 
    df %>%
      group_by(origin) %>%
      summarise(mean_delay = mean(dep_delay,na.rm =T))
    #retard moyen par mois 
    df %>%
      group_by(month) %>%
      summarise(mean_delay = mean(dep_delay,na.rm =T))
#speed flights depart around 11-13 by month   
 df %>%
   filter(dep_time >= 1100 & dep_time <= 1300) %>%
    group_by(month) %>%
   summarise(meandelay = mean(dep_delay,na.rm=T))
 
#what is the mean delay of flights by carrier ,for each month?
 df %>%
   group_by(carrier,month) %>%
   summarise(mean_delay = mean(dep_delay,na.rm=T))
#what is the maximum departure delay occured for each of the three NYC airports,bY each day?
 df %>%
   group_by(origin,month ,day) %>%
   summarise(maxdelay = max(dep_delay,na.rm=T))
#what is the mean 
 
 df %>%
   group_by(origin) %>%
   summarise(mean_haul = mean(air_time,na.rm=T))
 
###babynames
 install.packages("babynames")
 library(babynames) 
df <- babynames 
