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


### partons à l'éxploration des types de plots!!!

## 1 var, discrete
## mpg$manufacturer

# geom_bar
# équivalnet de table()
mpg %>% 
  ggplot()+
  aes(x=manufacturer, fill = drv)+
  geom_bar()

# ordonner les barres
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N = n()) %>%
  ggplot()+
  aes(x=reorder(manufacturer, N), fill = drv)+
  geom_bar()


## 1 var, continue

## histogram -- densité
## hwy
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_histogram()

## densité
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_density()

## boxplot
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_boxplot()

## violin plot
mpg %>% 
  ggplot()+
  aes(x = 1, y = hwy)+
  geom_violin()


### 2 variables

## 2 continues

# scatter: geom_point()
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

# et si tous les points sont au me endroit?
# jitter
# jitter ajoute du bruit blanc à la position des données
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_jitter(width = .3, height = .3)+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")


## 2 var, 1 continue 1 discrète
# conso en hwy par class

# plot vide
p <- mpg %>% 
  ggplot()+
  aes(x = class, y = hwy, color = class, fill = class)

p + geom_boxplot()

# et si  on voulait inverser les axes? 

p + coord_flip()+ geom_boxplot()

## violin plot

p + geom_violin()


## raincloud plot

p + geom_jitter(height = 0, width = .2)


### 2 variables, les deux sont discrètes

## class and drv

mpg %>% 
  ggplot() +
  aes(x = class, y = drv)+
  geom_count()

## équivalent à
table(mpg$class, mpg$drv)


### 3 variables

## geom_tile
## pour chaque drive, pour chaque N de cylindre, 
## quelle est la consommation moyenne?

mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
  ggplot()+
  aes(x = drv, y = cyl, fill = mean_conso)+
  geom_tile()

#### Exercices

library(nycflights13)

df <- flights

## exo 1
## barplot des 10 destinations les plus fréquentes à partir de JFK
# tip: use summarise(N = n())

## getting the data
data <- df %>% 
  filter(origin == "JFK") %>% 
  group_by(dest) %>% 
  summarise(N = n()) %>%   ## tally()
  top_n(10) 

# base solution
data %>% 
  ggplot() + 
  aes(dest, N)+
  geom_col()

# reordering by N
data %>% 
  ggplot() + 
  aes(x = reorder(dest, -N), N)+
  geom_col()


## exo 2
## distributions des vols sur le mois de janvier
df %>% 
  filter(month == 1) %>% 
  ggplot()+
  aes(x = day)+
  geom_density()

## exo 3
## barplot des 20 destinations les plus desservie, par aéroport
data <- df %>% 
  group_by(origin, dest) %>% 
  summarise(N = n()) %>% 
  top_n(20)

data %>% 
  ggplot() + 
  aes(x = dest, y = N, fill = origin)+
  geom_col()+
  facet_wrap(.~origin, scales = "free")+
  coord_flip()














