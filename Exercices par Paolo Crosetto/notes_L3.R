### notes prises en cours
## L3: ggplot base
## PC
library(tidyverse)
## explore datasaurus

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

# exploration textuelle
df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y),
            sd_x = sd(x), sd_y = sd(y))


##plotting

ggplot()

ggplot(mpg)

p <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()

# ajouter de la couleur
p + geom_point(aes(color = class, size = cyl, shape = manufacturer))


ggplot(mpg, aes(x = cty, y = hwy, color = class))+
  geom_point()+
  geom_smooth(inherit.aes = F)


## exercices
library(nycflights13)

df <- flights

## rel entre retard au départ ou à l'arrivée
df %>% 
  ggplot(aes(x = dep_delay, y = arr_delay))+
  geom_point()

## même chose mais juste pour juin
df %>% 
  filter(month == 6) %>% 
  ggplot(aes(x = dep_delay, y = arr_delay))+
  geom_point()

## est-ce que il y a des compagnies qui font plus de retard?
# comagnie -> couleur
df %>% 
  filter(month == 6) %>% 
  ggplot(aes(x = dep_delay, y = arr_delay))+
  geom_point(aes(color = carrier)) -> plot2

## facets
plot2 +
  facet_wrap(~day) ## si une seule dimension: utiliser WRAP

## 2-dimensioanl facets
plot2 +
  facet_grid(carrier~origin) ## syntaxe ligne ~ colonne 

  

## plots d'une seule variable

## continue

# density, simple
flights %>% 
  ggplot(aes(x = dep_time))+
  geom_density()
  
# with layers
flights %>% 
  ggplot(aes(x = dep_time))+
  geom_density(aes(color = origin))+
  facet_wrap(~origin)

## histogram

# simple
flights %>% 
  ggplot(aes(x = dep_time))+
  geom_histogram()
  
# complex
flights %>% 
  ggplot(aes(x = dep_time))+
  geom_histogram(aes(color = origin, fill = origin))+
  facet_grid(origin~.)

## une seule variable, discrète

# nombre de vols par compagnie, simple
flights %>% 
  ggplot(aes(carrier))+
  geom_bar()

#complex
flights %>% 
  ggplot(aes(carrier))+
  geom_bar(aes(color = origin, fill = origin))

## position des barres

# par défaut: empilées
flights %>% 
  ggplot(aes(carrier))+
  geom_bar(aes(color = origin, fill = origin))

# fréquences relatives
flights %>% 
  ggplot(aes(carrier))+
  geom_bar(aes(color = origin, fill = origin),
           position = position_fill())

# barres les unes à côté des autres
flights %>% 
  ggplot(aes(carrier))+
  geom_bar(aes(color = origin, fill = origin),
           position = position_dodge())


# DEUX variables

# cont, cont
# scatterplot -> nuage de points
mpg %>% 
  ggplot(aes(cty, hwy))+
  geom_point()

# scatterplot avec jitter
mpg %>% 
  ggplot(aes(cty, hwy))+
  geom_jitter()

# smooth -> ajouter une tendance
mpg %>% 
  ggplot(aes(cty, hwy))+
  geom_smooth()+
  geom_jitter()

# cont, disc

## consommation en ville par constructeur

## boxplot
mpg %>% 
  ggplot(aes(x = manufacturer, y = cty)) +
  geom_boxplot()

## violin
mpg %>% 
  ggplot(aes(x = drv, y = cty)) +
  geom_violin(aes(fill = drv))

## barres -> colonnes
# nombre de vols par compagnie aérienne

# version 1: une seule variable -> geom_bar
flights %>% 
  ggplot(aes(carrier))+
  geom_bar() #compte les observations

# version 2: avec deux variables -> geom_col
flights %>% 
  group_by(carrier) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(carrier,-n), y = n))+
  geom_col()

## quelle compagnie a plus de retard en moyenne?
flights %>% 
  group_by(carrier) %>% 
  summarise(mean_delay = mean(arr_delay, na.rm = T)) %>% 
  ggplot(aes(x = reorder(carrier,-mean_delay), y = mean_delay)) + 
  geom_col()

## pas forcément catégorielel sur les x
flights %>% 
  group_by(carrier) %>% 
  summarise(mean_delay = mean(arr_delay, na.rm = T)) %>% 
  ggplot(aes(y = reorder(carrier,mean_delay), x = mean_delay)) + 
  geom_col()

# pas forcéement geom_col
# on peut avec des points
flights %>% 
  group_by(carrier) %>% 
  summarise(mean_delay = mean(arr_delay, na.rm = T)) %>% 
  ggplot(aes(y = reorder(carrier,mean_delay), x = mean_delay)) + 
  geom_point()


## deux var, discrète, discrète

## est-ce certains producteurs se concentrent sur certains segments du marché

mpg %>% 
  ggplot(aes(manufacturer, class)) + 
  geom_count()
  
## quelles compagnies partent de quel aéroport

flights %>% 
  ggplot(aes(y = carrier, x = origin))+
  geom_count()

## 3 variables (!!)

# geom_tile

# nombre de vols par origine et par destination

flights %>% 
  group_by(origin, dest) %>% 
  summarise(n = n()) %>% 
  filter(n > 1000) %>% 
  ggplot(aes(x = dest, y = origin, z = n, fill = n)) +
  geom_tile()

