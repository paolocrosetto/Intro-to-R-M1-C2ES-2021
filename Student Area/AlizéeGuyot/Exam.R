# CONTRÔLE
library(tidyverse)

scoobydoo <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

summary(scoobydoo)

# nb d'épisodes répertoriés
dim(scoobydoo)[1]

# noms des différents types de formats
table(scoobydoo$format)

scoobydoo %>% 
  ggplot() +
  aes(x="", y = format, fill=format)+
  geom_col() +
  coord_polar(theta = "y")


# qui a attrapé combien de monstres 

X <- c(sum(scoobydoo$caught_fred==TRUE),
       sum(scoobydoo$caught_daphnie==TRUE),
       sum(scoobydoo$caught_velma==TRUE),
       sum(scoobydoo$caught_shaggy==TRUE),
       sum(scoobydoo$caught_scooby==TRUE),
       sum(scoobydoo$caught_not==TRUE),
       sum(scoobydoo$caught_other==TRUE))  

names <-c("Fred","Daphnie","Velma","Shaggy","Scooby","No-one","Other")
caught <- cbind.data.frame(X, names)

caught %>% 
  ggplot(aes(names, X, fill=names))+
  geom_col()+
  labs(title = "Qui a attrapé combien de monstres", x = "", y="")

# qui a été capturé combien de fois

X <- c(sum(scoobydoo$captured_fred==TRUE),
       sum(scoobydoo$captured_daphnie==TRUE),
       sum(scoobydoo$captured_velma==TRUE),
       sum(scoobydoo$captured_shaggy==TRUE),
       sum(scoobydoo$captured_scooby==TRUE))

names <-c("Fred","Daphnie","Velma","Shaggy","Scooby")
captured <-cbind.data.frame(X,names)

captured %>% 
  ggplot(aes(names, X, fill=names))+
  geom_col()+
  labs(title = "Qui a été capturé combien de fois", x = "", y="")

# qui a démasqué combien de fois

X <- c(sum(scoobydoo$unmask_fred==TRUE),
       sum(scoobydoo$unmask_daphnie==TRUE),
       sum(scoobydoo$unmask_velma==TRUE),
       sum(scoobydoo$unmask_shaggy==TRUE),
       sum(scoobydoo$unmask_scooby==TRUE),
       sum(scoobydoo$unmask_other==TRUE)) ;X

names <-c("Fred","Daphnie","Velma","Shaggy","Scooby","Other")
unmask <-cbind.data.frame(X,names)

unmask %>% 
  ggplot(aes(names, X, fill=names))+
  geom_col()+
  labs(title = "Qui a demasqué combien de fois", x = "", y="")



# durée en fonction du type
ggplot(scoobydoo, aes(run_time, format)) + geom_point(aes(color = format))

# platforme en fct des saisons
ggplot(scoobydoo, aes(season, network)) + geom_point()


# Moyenne des notes en fonction du format
scoobydoo %>% 
  group_by(format) %>% 
  summarise(moy = mean(as.numeric(imdb),na.rm=T))

# Moyenne des vues en fonction du format
scoobydoo %>% 
  group_by(format) %>% 
  summarise(moy = mean(as.numeric(engagement),na.rm=T))

sum(as.numeric(scoobydoo$monster_amount))

scoobydoo %>% 
  ggplot() +
  aes(x = culprit_amount, y = suspects_amount, color = monster_real) +
  geom_point()

scoobydoo %>% 
  ggplot() +
  aes(culprit_amount, monster_amount, color=culprit_gender) +
  geom_point()












scoobydoo <- scoobydoo %>%
  mutate(monsters_gen = case_when(
    str_detect(monster_gender, "Male") & str_detect(monster_gender,"Female") ~ "Both",
    str_detect(monster_gender, "Male") ~ "Male",
    str_detect(monster_gender, "Female") ~ "Female",
    TRUE ~ "NoGender"))

ggplot(scoobydoo) +
  geom_point(aes(x = date_aired, y = monster_amount, color = monster_real)) + 
  facet_wrap(~ monsters_gen)
