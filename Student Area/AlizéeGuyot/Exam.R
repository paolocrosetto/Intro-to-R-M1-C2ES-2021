# CONTRÔLE
library(tidyverse)
library(lubridate)

scoobydoo <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

summary(scoobydoo)

scoobydoo <- scoobydoo %>%
  mutate(genre = case_when(
    str_detect(monster_gender, "Male") & str_detect(monster_gender,"Female") ~ "Mixte",
    str_detect(monster_gender, "Male") ~ "Masculin",
    str_detect(monster_gender, "Female") ~ "Féminin",
    TRUE ~ "Pas de Genre")) %>% 
  mutate(year = year(date_aired)) %>% 
  mutate(imdb2 = as.numeric(imdb))


# nb d'épisodes répertoriés
dim(scoobydoo)[1]

# noms des différents types de formats
table(scoobydoo$format)

scoobydoo %>% 
  ggplot() +
  aes(x="", y = format, fill = format) +
  geom_col() +
  coord_polar(theta = "y")

scoobydoo %>% 
  ggplot() +
  aes(x=format, fill = format) +
  geom_bar()

# qui a attrapé combien de monstres 

X <- c(sum(scoobydoo$caught_fred==TRUE),
       sum(scoobydoo$caught_daphnie==TRUE),
       sum(scoobydoo$caught_velma==TRUE),
       sum(scoobydoo$caught_shaggy==TRUE),
       sum(scoobydoo$caught_scooby==TRUE))  

names <-c("Fred","Daphnie","Velma","Shaggy","Scooby")
caught <- cbind.data.frame(X, names)

c <- caught %>% 
  ggplot(aes(names, X, fill=names))+
  geom_col()+
  labs(title = "Qui a attrapé combien de monstres", x = "", y="")

# qui a été capturé combien de fois

Y <- c(sum(scoobydoo$captured_fred==TRUE),
       sum(scoobydoo$captured_daphnie==TRUE),
       sum(scoobydoo$captured_velma==TRUE),
       sum(scoobydoo$captured_shaggy==TRUE),
       sum(scoobydoo$captured_scooby==TRUE))

captured <-cbind.data.frame(Y,names)

b <- captured %>% 
  ggplot(aes(names, Y, fill=names))+
  geom_col()+
  labs(title = "Qui a été capturé combien de fois", x = "", y="")

# qui a démasqué combien de fois

Z <- c(sum(scoobydoo$unmask_fred==TRUE),
       sum(scoobydoo$unmask_daphnie==TRUE),
       sum(scoobydoo$unmask_velma==TRUE),
       sum(scoobydoo$unmask_shaggy==TRUE),
       sum(scoobydoo$unmask_scooby==TRUE))

unmask <-cbind.data.frame(Z,names)

a <- unmask %>% 
  ggplot(aes(names, Z, fill=names))+
  geom_col()+
  labs(title = "Qui a demasqué combien de fois", x = "", y="")


scoobydoo %>% 
  ggplot()+
  aes(x = names, fill = names) +
  geom_bar()



a
b
c



# durée en fonction du type
ggplot(scoobydoo, aes(run_time, format)) + geom_point(aes(color = format))

# note en fct de la durée
ggplot(scoobydoo, aes(run_time, imdb2)) + geom_point(aes(color = format))

# platforme en fct des saisons
ggplot(scoobydoo, aes(season, network)) + geom_point()


# Moyenne des notes en fonction du format
scoobydoo %>% 
  group_by(format) %>% 
  summarise(moy = mean(imdb2,na.rm=T))

# Moyenne des vues en fonction du format
scoobydoo %>% 
  group_by(format) %>% 
  summarise(moy = mean(as.numeric(engagement),na.rm=T))


# Moyenne des notes en fonction des saisons
scoobydoo %>% 
  group_by(season) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

reg <- lm(as.numeric(imdb) ~ season, data = scoobydoo)
summary(reg)
plot(reg)

reg2 <- lm(as.numeric(engagement) ~ season + imdb2, data = scoobydoo)
summary(reg2)
plot(reg2)

reg3 <- lm(non_suspect ~ arrested, data = scoobydoo)


summary(scoobydoo$non_suspect)

sum(as.numeric(scoobydoo$monster_amount))


#osef
scoobydoo %>% 
  ggplot() +
  aes(x = culprit_amount, y = suspects_amount, color = monster_real) +
  geom_point()

#osef
scoobydoo %>% 
  ggplot() +
  aes(culprit_amount, monster_amount, color=culprit_gender) +
  geom_point()


## Note en fct que le monstre soit réel ou non

scoobydoo %>% 
  ggplot() +
  aes(year, imdb2, fill = monster_real) +
  geom_col()
## problème avec imdb2!!!

## Note moyenne en fct de fred à attrapé le monstre

cf <- scoobydoo %>% 
  filter(captured_fred==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

cv <- scoobydoo %>% 
  filter(captured_velma==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

cd <- scoobydoo %>% 
  filter(captured_daphnie==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

csc <- scoobydoo %>% 
  filter(captured_scooby==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

csh <- scoobydoo %>% 
  filter(captured_shaggy==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

CAP <- cbind(cf, cd, cv, csh, csc)

colnames(CAP) <- names

CAP

## Genre en fonction des années et du fait qu'il soit réels



ggplot(scoobydoo) +
  geom_point(aes(x = year, y = monster_amount, color = monster_real)) + 
  facet_wrap(~ genre)
