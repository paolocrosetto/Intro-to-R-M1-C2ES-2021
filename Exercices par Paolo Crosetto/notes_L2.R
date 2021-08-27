library(tidyverse)

# cette ligne importe des données sur les aéroports de
library(nycflights13)

# facon 1 d'importer: à partir d'un package
df <- flights

# facon 2 d'importer: à partir d'un fichier
hdi <- read_csv("Data/HDIdata.csv")

#descriptif des données
summary(df)

# alternative pour summary des donnée
install.packages("skimr")
library(skimr)
skim(df)

## filter
# cela permet de sélectionner des lignes

# tous les vols de décembre
filter(df, month == 12)

# tous les vols de décembre qui partent de JFK
filter(df, month == 12 & origin == "JFK")

# tous les vols de décembre qui partent de JFK et qui vontà LAX
vol_jfk_lax_decembre <- filter(df, month == 12 & origin == "JFK" & dest == "LAX")

# tous les vols de janvier qui partent de JFK et qui ne vont pas à LAX
filter(df, month == 1 & origin == "JFK" & dest != "LAX")

##arrange
# ordonner les données par variable

arrange(df, month, day, dep_delay)

##select
# sélectionner des colonnes
select(df, month, day, dep_delay)

#de-sélectionner des colonnes
select(df, -month, -year)

#aide à la sélection
select(df, starts_with("arr"))
select(df, ends_with("y"))
select(df, contains("y"))
select(df, everything())
select(df, arr_delay, dep_delay, everything())

#sauvegarder dans un objet les délais 
delays <- select(df, contains("delay"))

#rename
#renomme les variables
rename(df, mois = month, annee = year, jour = day)


#mutate
# modifier des variables

## calculaer la vitesse des avions en miles/minute
df2 <- mutate(df, speed_miles_minute = distance/air_time)

select(df2, speed_miles_minute, everything())

#calculer la vitesse en km/h
df3 <- mutate(df2, speed_km_h = speed_miles_minute*60/1.60934)

select(df3, starts_with("speed"))


## enter the PIPE!
## %>%
## %>%

## pour tous les vols de décembre montrer juste le delay
df %>% 
  filter(month == 12) %>% 
  select(month, contains("delay"))

## calucler vitesse en km/h
df %>% 
  select(air_time, distance) %>% 
  mutate(distance_km = distance*1.6, time_h = air_time/60) %>% 
  mutate(speed_km_h = distance_km/time_h)

# summarise (summarize)
# #moyenne de la vitesse des avion
df_speed %>% 
  summarise(mean = mean(speed_km_h, na.rm = TRUE))

# meme chose du début
df %>% 
  select(air_time, distance) %>% 
  mutate(distance_km = distance*1.6, time_h = air_time/60) %>% 
  mutate(speed_km_h = distance_km/time_h) %>% 
  summarise(mean = mean(speed_km_h, na.rm = TRUE))

#vitesse maximale dans la base
df_speed %>% 
  summarise(max = max(speed_km_h, na.rm = TRUE))

## group_by()
## permet de faire des groupes
## summarise va obéir à group_by()

##quelle compagnie aérienne va plus vite?
df %>% 
  select(air_time, distance, carrier) %>% 
  mutate(speed = distance/air_time) %>% 
  group_by(carrier) %>% 
  summarise(maxspeed = mean(speed, na.rm = TRUE)) %>% 
  arrange(-maxspeed)

#pourquoi? peut etre c'est lié aux distances?
df %>% 
  select(air_time, distance, carrier) %>% 
  mutate(speed = distance/air_time) %>% 
  group_by(carrier) %>% 
  summarise(meanspeed = mean(speed, na.rm = TRUE),
            meandist = mean(distance, na.rm = TRUE)) %>% 
  arrange(-meanspeed)
  

# what is the mean delay of flights by carrier, for each month?
df %>% 
  select(arr_delay, month, carrier) %>% 
  group_by(month, carrier) %>% 
  summarise(mean_delay = mean(arr_delay, na.rm = T))

# from which airport do the longer haul flights depart?
df %>% 
  select(origin, distance) %>% 
  group_by(origin) %>% 
  summarise(reponse = mean(distance, na.rm = T),
            vairance = sd(distance, na.rm = T))
