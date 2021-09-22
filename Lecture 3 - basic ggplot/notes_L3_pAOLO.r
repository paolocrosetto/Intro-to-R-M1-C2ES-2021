### notes L3 Plotting

## exercice sur la viz de donnees

library(tidyverse)

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

table(df$dataset)

## calcule moyenne et dev st de chaque dataset

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y),
            sd_x = sd(x), sd_y = sd(y))

## plotter les dataset

df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  facet_wrap(~dataset)


### GGPLOT!

## data used for examples:
mpg <- mpg

# appeleler la fonction crée un objet plot
ggplot(mpg)


# aes crée les axs et n'impprte quel mapping des données aux élements graphiques

p <- ggplot(mpg, aes(x = displ, y = hwy))

# opératuer "+": ajouter d'autres éléments au plot
p <- p + geom_point()

p + geom_smooth()

p + geom_point() + geom_smooth()

## smoother linéaire
p + geom_point() + geom_smooth(method = "lm")

# add reference line
p + geom_point() + geom_hline(yintercept = 25, color ='red')


## using AES for other purposes than axes

# couleur
p + aes(color = class)

# size (taille)
p + aes(size = cyl)

# both
p + aes(color = class, size = cyl)


# shape
p + aes(shape = fl)


## exercice babynames!
library(babynames)

#evolution du nom "Mary" aux EE UU

# Jennifer
babynames %>% 
  filter(name == "Jennifer" & sex == "F") %>% 
  ggplot()+
  #assigner des variables aux axes
  aes(x = year, y = prop)+
  #on ajouteun objet géometrique
  geom_line()


### facets
p
p + facet_wrap(~trans)
# _wrap est une structure unidimensionnelle qui "va à la ligne"
# _grid est une grille bdimensionnelle (row~column)

## detail de facet

## 1. facet_wrap vs facet_grid
p + facet_grid(trans~.)

p + facet_grid(.~trans)

## grid allows for 2 dimensions

p + facet_grid(drv~class)

## example avec babynames

# la dynamique des 2 noms les plus utilisés en 1880
# un pour les filles un pour les garçons

babynames %>% 
  filter(year == 1880) %>% 
  arrange(-prop)

## ourrah c'est JOhn et Mary on va avoir un Jésus

babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot()+
  aes(x = year, y = prop, color = sex)+
  geom_line()+
  facet_wrap(~name)


