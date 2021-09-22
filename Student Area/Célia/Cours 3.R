
### Cours 3 : Plotting

## exercice sur la visualisation de données

library(tidyverse)

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

table(df$dataset)

## calcule moyenne et dev

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y),
            sd_x = sd(x), sd_y = sd(y))

# les stats sont toutes les mêmes pour les ensembles de donneés alors que quand
# on plot on se rend compte qu'elles sont très différentes.

# Mais attention il est très facile de mentir avec les données : un graphique
# doit être beau mais surtout honnête.

# Ne pas mettre de 3D inutile (on ne met jamais de 3D normalement). 

# pour faire un bon graphique, il faut d'abord penser au message qu'on veut 
# envoyer. Ensuite on le dessine à la main pour voir à quoi il doit ressembler.
# On dessine ensuite les axes puis on choisit le type de graphique. 

# Pour faire des plots on utilise le package ggplot2 (déjà dans tidyverse).
# ggplot : gg pour grammar of graphics.
# On va utiliser le dataset mpg.

mpg <- mpg

# créer un objet plot
ggplot(mpg) # crée un plot vide (début de la phrase avec le sujet mpg)

# créer les axes avec displ en abcisse et hwy en ordonnée
p <- ggplot(mpg, aes(x = displ, y = hwy))
p

### opérateur "+" : ajouter d'autres éléments au plot

p + geom_point() # crée un graphique avec des points 

p + geom_smooth()

p + geom_point() + geom_smooth()

## smopther linéaire 
p + geom_point() + geom_smooth(method = "lm")

## ad reference line 
p + geom_point() + geom_hline(yintercept = 25, color = 'red')


## utilisation de aes pour changer l'apparence des points (ou autre truc)

p <- p + geom_point()
p + aes(color = class) # place les points de la variable class en couleur sur
                       # le graphique p

p + aes(size = cyl)

p + aes(color = class, size = cyl)

p + aes(shape = fl)


### exercice
library(babynames)

## évolution du nom Mary aux EU

# sans plot
babynames %>% 
  filter(name == "Mary" & sex == "F") -> Maries
# "Maries" contient l'évolution du nom "Mary" pour les filles

Maries %>% 
  ggplot()+
  # assigner des variables aux axes
  aes(x = year, y = prop)+
  # on ajoute un objet géométrique
  geom_line()


### facets

# Quand les phrases deviennent trop longues on les coupe. 

p <- ggplot(mpg, aes(x = displ, y = hwy))
p <- p + geom_point()
p + facet_wrap(~class) #crée différents graphiques par classe (équivalent à des 
# groupes). Les graphiques sont répartis sur la page divisée horizontalement et
# verticalement

# détail de facet
# 1. facet_wrap vs facet_grid
p + facet_grid(trans~.) # pareil que facet_wrap mais en lignes


# exemple avec babynames
# la dynamique des 2 noms les plus utilisés en 1880 (1 fille et 1 garçon)

babynames %>% 
  filter(year == 1880) %>% 
  arrange(-prop)

babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot()+
  aes(x = year, y = prop, color = sex) +
  geom_line()+
  facet_wrap(~name)

### les types de plots

## 1 seule variable discrète 
## exemple : mpg$manufacturer

mpg %>% 
  ggplot()+
  aes(x=manufacturer)+
  geom_bar() # équivalent de table car il compte le nb de rep des valeurs

mpg %>% 
  ggplot()+
  aes(x=manufacturer, fill=manufacturer)+ # rempli les colonnes avec couleurs
  geom_bar()


mpg %>% 
  ggplot()+
  aes(x=manufacturer, fill=drv)+ # couleur en fct d'une autre variable
  geom_bar()

# ordonner les barres 

mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N=n()) %>% 
  ggplot()+
  aes(x=reorder(manufacturer, -N), fill=drv)+
  geom_bar()
# reorder range les colonnes (le "-" les met dans l'ordre décroissant)

## 1 variable continue

## histogramme -- densité
## hwy
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_histogram() # avec bins par défaut

mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_histogram(bins = 4) # on peut changer bins

mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_density()

mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_density(adjust = 5) # pour normaliser les données (petit : normal, grand:
                           # suit les donnée)

## boxplot
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_boxplot()

## violin plot : chacun des violons est une densité
mpg %>% 
  ggplot()+
  aes(x=class, y = hwy)+
  geom_violin()

mpg %>% 
  ggplot()+
  aes(x=1, y = hwy)+ #triche pour montrer un seul violon
  geom_violin()

## 2 variables 

## 2 continues

#scatter
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
# nous apprend que les voitures consomment plus sur l'autoroute qu'en ville


# et si tous les points sont au même endroit ? 
# jitter
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_jitter()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_jitter(width = 1, height = 1)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")
# ajoute des points qui n'existent pas vraiment mais qui peuvent aider dans 
# certains cas à visualiser


## 1 variable continue et 1 discrète

#conso en hwy par class

#boxplot
p <- mpg %>% 
  ggplot()+
  aes(x = class, y = hwy, color = class, fill = class)

p + geom_boxplot()

# et si on voulait inverser les axes ? 

p + coord_flip()+geom_boxplot()

# violons

p + geom_violin()

# raincloud plot

p + geom_jitter(height = 0, width = .2) # permet de se rendre compte du nombre 
# de données dans chaque classe


## 2 variables : les 2 sont discrètes

# drv and class
mpg %>% 
  ggplot()+
  aes(x = class, y = drv)+
  geom_count()

## équivalent à 
table(mpg$class, mpg$drv)


## 3 variables
# on n'utilise pas la 3D car très peu lisible

# geom_tile
# pour chaque drv, pour chaque N de cylindre, quelle est la conso moyenne ?
mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
  ggplot()+
  aes(x = drv, y = cyl, fill = mean_conso) %>% 
  geom_tile()



### Exercices

library(nycflights13)

df <- flights

## exo 1 : barplot des 10 destinations les plus fréquentes à partir de JFK
df %>% 
  filter(origin == "JFK") %>% 
  group_by(dest) %>% 
  summarise(N = n()) %>% # équivalent à tally()
  top_n(10) %>% 
  ggplot()+
  aes(x = reorder(dest, -N), N)+
  geom_col() #on ne peut pas utiliser bar car on compte nous meme les données

## ex2 : distribution des vols sur le mois de janvier
df %>% 
  filter(month == 1) %>% 
  group_by(day) %>% 
  summarise(N = n()) %>% 
  ggplot()+
  aes(day, N)+
  geom_col()

## exo 3 : barplot des 20 destinations les plus desservies par aéroport


























































