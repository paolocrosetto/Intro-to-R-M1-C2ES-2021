##On est là je pense Monsieur.
##attach package to working area
library(nycflights13)
library(tidyverse)

flights

df<-flights
df

#stat des

summary(df)


#exploration des données avec skimr


install.packages("skimr")

#skimming
skimr::skim(df)

library(tidyverse)

#filter
filter(df,month == 6 & day == 1)# june 1st flights

#variables names

names(df)

#tabulate data
table(df$origin)


#combiens de vols sont partis de jfk le 7 juillet 2013?

filter(df,month == 7 & origin == "JFK"& day == 7)

#better

filter(df,month %in% c(3,4,5,6))


## ARRANGE -> ORDER DATA

# ordonner par heure de départ 
arrange(df,dep_time)

arrange(df,-sched_dep_time)

arrange(df,origin,month,dep_time)

### SELECT  -- selectionner des variables 

#origin and destination only
select(df,month,dest)

#"drop" month and day
select(df, -month,-day)

## helper functions 

# all variables starting or ending with something
## only arrival data
select(df, starts_with("arr"))
## only time data
select(df, ends_with("time"))
# select with "contains"

# toutes les variables qui contiennent un "t"
select(df , contains("t"))

#exercice

## trouvez une base de données de tous les vols partis de jfk le 4 mai 2013
# sortez la destination uniquement

#solution
cellule <-filter(df , origin ==  "JFK" , month == 5, day == 4 )

cellule

select(cellule, dest)

# solution PIPE
df %>% 
  filter(origin ==  "JFK" , month == 5, day == 4) %>%
  select(dest)
#taper le pipe : CRTL+MAJ+M

#LISTEZ Les vols patrtis 

df %>%
  filter(month == 7 & dep_time >=1200 & dep_time <= 1400 & day == 3)%>%
  arrange(origin)%>%
  select(arr_time,dep_time)
  
#renommer
rename(df, mois = month)

#ou
df%>%
  rename(mois = month , jour = day , année = year)

# MUTATE
## CHange le contenu des variables, en créant des nouvelles variables
## normalize year to years from now 
df %>%
  mutate(year_from_now =2021 - year)%>%
  select(year, year_from_now)

## creez unjafge "vitesse" et une "year from now")
df%>%
  mutate(year_from_now =2021 - year, speed =  distance/ air_time)%>%
  select(year_from_now, distance, air_time, speed)

## creez une variable vitesse qui soit comprehensible aux EU)
df%>%
  mutate(mph = 60*distance/ air_time,kmh = mph/1.6)
## mutate avec fonctions logique 
## variable qui nous dit si l'aeroport est jfk
df%>%
  mutate(isJFK =  origin == JFK)%>%
  select(origin, isJFK)

## SUMMARIsE : prend unn vecteur , retourne une valeur 
# retard moyen des avions 

df%>%
 summarise (mean_delay_depart = mean(dep_delay, na.rm = TRUE),
            mean_delay_arrive = mean(arr_delay, na.rm = TRUE),
            sd_delay_depart =  sd(dep_delay, na.rm = TRUE))

## Group-by : grouper les observations

## groupement simple

## grouper par aéroport d'origine

df%>%
  group_by(origin)

## grouper par plusieurs variables 
## month and origin
df%>%
  group_by(origin, month)

# summarise and group by
## Retard moyen par aeroport

df%>%
  group_by(origin)%>%
  summarise(mean_delay = mean(dep_delay, na.rm=T))
## Retard moyen par month

group_by(month)%>%
  summarise(mean_delay = mean(dep_delay, na.rm=T))


## speed flights depart around 11-13 by month?

df%>%
  filter(dep_time >=1100 & dep_time <= 1400)%>%
  group_by(month)%>%
  summarise(mean_delay = mean(dep_delay,na.rm=T))
## Exo1
df %>%
  group_by(carrier, month) %>%
  summarise(mean_delay = mean(dep_delay, na.rm=T))
## Exo2
df%>%
  
   group_by(origin,month ,day)%>%
  summarise(maxdelay =  max(dep_delay, na.rm = T))
## Exo3 
df%>%
  group_by(origin)%>%
  summarise(meandelay= mean(air_time,na.rm =  T ))

## 4 babynames
install.packages("babynames")
library(babynames)
df<-babynames


df %>%
  filter(year == 1910)%>%
  group_by(sex)%>%
  top_n(1)
