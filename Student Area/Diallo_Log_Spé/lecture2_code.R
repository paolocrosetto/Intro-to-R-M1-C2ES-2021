#attche package to working area
library(nycflights13)
library(tidyverse)
# import data space 
df <- flights
View(flights) 
#get informations on data 
#statistiques descriptives
summary(flights)

# exploration des données avec skimr
install.packages("skimr")
skimr::skim(df)

library(tidyverse)
install.packages("tidyverse")

#filter 
#one condition
filter(df, month == 6)  # june flights
# more condition 
filter(df, month == 6 & day == 1)  # june 1st fligts

# Combien de vols partis de JFK le 7 juillet 2013
filter(df, month == 7 & day == 7 & origin = "JFK" & year == 2013 )
# vol parts en juillet ou en aout 
filter(df, month == 7 | month == 8)

# vols de mars à juin
filter(df, month == 3 | month == 4| month == 5 |month == 6 )
# better
filter()
# variable names 

# ordonner par heure de départ
arrange(df, sched_dep_time)
#filter(ligne) select(colonne)



  # SELECT
#origin and destination only
select(df,origine,dest)

#"drop" month and day
select(df, -month, -day)

####helper funtcion
  # All variable startinf or endind with something
  # only arrival data
select(df, starts_With("are"))
  # only time data
select(df, ends_With("time"))

#select with "contains"
#toutes les variables qui contienne un "t"
select(df, contains("t"))

 #------- --------EXERCICES ---------------#

#toutes la base de données de tous les vols partis de JFK le 4 mai 2013  
#sorter la destination uniquement
      #solution 1
exo1 <- filter(df,origin == "JFK" & month == 5 & day == 4)
select(exo1,dest)
     # solution 2
exo1 <- filter(df,origin == "JFK" & month == 5 & day == 4),dest)*
     
  # solution 3 : PIPE
  df %>%
    filter(origin == "JFK" & month == 5 & day == 4) %>%
    select(dest)
   #PIPE : CTRL+MAJ+M
#-----------------------EXERCICES POUR LA PAUSE-------------
  #lister les vols partis entre midi et 14h le 3 juillet
  #montrer moi juste l'heure de dépaer et d'arrivée
 #ordonner les données par aéroport
  df %>%
   filter(df, dep_time >= 1200 &dep_time <= 1400 & month == 7 & day == 3) %>%
   arrange(origin) %>%
   select(dep_time,arr_time)


#-----------RENAMING variable: rename
rename(df, moi = month)
df %>%
  rename(mois = month, jour = day, année = year)
#----------creating new variable: mutate()
   #change le contenu des variablesn,en créant des nouvelles variables
   
   #normaliser year to years from now
df %>%
  mutate(year_from_now = 2021 - year) %>%
  select(year,year_from_now)
  #créer une variable "vitesse" et une "year from now"
df %>%
  mutate(year_from_now = 2021 - year,
         speed = distance / air_time) %>%
  select(year,year_from_now,distance,air_time,speed)

  # créer une variable "vitesse" qui soit compréhensible aux EU
df %>%
  mutate(mph = 60*(distance / air_time),
         kmh =mph*1.6)
 # mutate crée des nouvelles variables,il les stocke à la fin du DF
 # mutate avec des fonctions logiques
 # nvariable qui nous dit si aéroport est JFK

#------------Summarise your data : summarize() ----------
 #retard moyen des avions
df %>%
  summarise(mean_delay_depart = mean(dep_delay, na.rm = TRUE,
           mean_delay_arrive = mean(dep_delay,na.rm = T),
           sd_delay_depart = sd(dep_delay,na.rm = T)))

#-----------GROUP-BY : grouper les observations--------
     #----groupement simple
df %>%
  group_by(origin)

df %>%
  group_by(origin,month)
     # combinaison de summarize et de group by
#quel est le retard moyen par aeroport
df %>%
  group_by(origin) %>%
  summarise(meandelay= mean(dep_delay,na.rm = T))
 # mean delay by month
df %>%
  group_by(origin) %>%
  summarise(meandelay= mean(dep_delay,na.rm = T))

#--------------------EXERCICE COMPLEXE---
df %>% 
  filter(df, dep_time > 1100 & dep_time < 1300) %>%
  group_by(month) %>%
  summarise(meandelay= mean(dep_delay,na.rm = T))

  

#------learning to use the PIPE--------
  #__ what is the mean of flight by carrier 
df %>% 
  group_by(carrier,month) %>%
  summarise(meandelay= mean(dep_delay,na.rm = T))

 #__what is the maximum departure delay occurred for each of the three NYC airports, by each day?
df %>% 
  group_by(carrier,month,day) %>%
  summarise(maxdelay=max(dep_delay,na.rm = T))
 
#__- what is the mean air time for each of these three airports? from which airport do the longer haul flights depart?
df %>% 
  group_by(origin) %>%
  summarise

#---BABYNAMES!!!!!
install.packages("babynames")
library(babynames)
df <- babynames
df %>%
  filter(year == 2010) %>%
  group_by(sex) %>%
  top n(1)

