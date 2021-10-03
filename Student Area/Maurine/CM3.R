### CM3 

# Plotting

## Exercice sur la visualisation de données

library(tidyverse)

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")
df
table(df$dataset)

## Calcule moyenne et dev st de chaque dataset

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y),
            sd_x = sd(x), sd_y= sd(y))

## Plotter les dataset

df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  facet_wrap(-dataset)


### GGPLOT

mpg <- mpg

# On appelle la fonction qui créer un objet plot
p <- ggplot(mpg, aes(x = displ, y = hwy))
summary(mpg)

# aes créer les axes et n'importe quel mapping des données aux élèments graphiques

# Opérateur "+" : permet d'ajouter d'autres élèments au plot
p <- p + geom_point()
p + geom_smooth()
p + geom_point() + geom_smooth()

# Smoothe pour rendre la pente linéaire
p + geom_point() + geom_smooth(method = lm)

# On ajoute une ligne de référence
p + geom_point() + geom_hline(yintercept = 25, color = 'purple')

## On utilise AES pour rajouter d'autres critères

# Couleur
p + aes(color = class)

# Taille
p + aes(size = cyl)

# Both
p + aes(color = class, size = cyl)

# Shape
p + aes(shape = fl)

### Exercice sur Babynames
library(babynames)

# Evolution du nom "Mary" aux EU
babynames %>% 
  filter(name == "Mary", sex == "F") -> Maries

# Maries contient l'évolution du nom "Mary" pour les filles
Maries %>% 
  ggplot() +
  # Assigner des variables aux axes
  aes(x = year, y = prop) +
  # on ajoute un objet geométrique
  geom_line()


## Facets
p
p + facet_wrap(~trans)
# _wrap est une structure unidimensionnelle qui va à la ligne
# _grid est une grille bidimensionnelle (ligne~colonne)
## Detail de facet
 ### facet_wrap vs facet_grid
p + facet_grid(trans~.)
p + facet_grid(.~trans)

 ### grid allows for 2 dimensions
p + facet_grid(drv~class)

### Exercice sur Babynames
library(babynames)

# La dynamique des 2 noms les plus utilisés en 1880
# Avec un pour les filles et un autre pour les garçons

babynames %>% 
  filter(year == 1880) %>% 
  arrange(-prop)

babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot() +
  aes(x = year, y = prop, color = name) +
  geom_line() +
  facet_wrap(~sex)


### Exploration des types de plots

## 1. variable discrète
#mpg$manufacturer
mpg %>% 
  ggplot() +
  aes(x = manufacturer, fill = drv) +
  geom_bar()

# La fonction geom_bar est equivalente à table()
table(mpg$manufacturer)

# On ordonne les barres
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N = n()) %>% 
  ggplot() +
  aes(x = reorder(manufacturer, -N), fill = drv)+ # le "-" va permettre de classer par ordre décroissant
  geom_bar()

## 2. variable continue
# Histogram --> densité
## On choisit hwy
mpg %>% 
  ggplot() +
  aes(x = hwy) +
  geom_histogram()

## On peut faire la densité
mpg %>% 
  ggplot() +
  aes(x = hwy) +
  geom_density(adjust = .4)

## On peut faire un boxplot
mpg %>% 
  ggplot() +
  aes(x = hwy) +
  geom_boxplot()

## On peut faire un violin plot
mpg %>% 
  ggplot() +
  aes(x = class, y = hwy) +
  geom_violin()

## 3. 2 variables continues
# Scatter plot avec un geom_point
mpg %>% 
  ggplot(aes(x = hwy, y = cty)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "pink", linetype = "dashed")

# Ici tous les points sont au même endroit
# On utilise donc Jitter (on ajoute de l'aléatoire aux données)
mpg %>% 
  ggplot(aes(x = hwy, y = cty)) +
  geom_jitter(width = .1, height = .1) +
  geom_abline(intercept = 0, slope = 1, color = "pink", linetype = "dashed")

# Cela nous permet de voir un peu mieux les points 

## 4. 2 variables : 1 continue et 1 discrète
# Consommation en autoroute par class
# Boxplot
p <- mpg %>% 
  ggplot() +
  aes(x = class, y = hwy, color = class, fill = class)

p + geom_boxplot()

# Si on veut inverser les axes, il faut la fonction coord_flip
p + coord_flip() + geom_boxplot()

# Avec violin plot cette fois-ci
p + geom_violin()

# Raincloub plot résoud les problèmes concernant la transparence de données existante avec le boxplot et le violin plot
p + geom_jitter(height = 0, width = .2)

## 5. 2 variables discrètes
# Avec comme variables exemples : drv et trans
mpg %>% 
  ggplot() +
  aes(x = class, y = drv) +
  geom_count()

# La fonction geom_count est equivalente à table()
table(mpg$class, mpg$drv)

## 6. 3 variables
# Pour chaque drive, pour chaque N de cylindre, quelle est la consommation moyenne ?
# On utilise geom_tile

mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(mean_conso =  mean(hwy)) %>% 
  ggplot() +
  aes(x = drv, y = cyl, fill = mean_conso)+
  geom_tile()

# geom_hex peut repoduire la même chose mais en hexagone


### Exercices
library(nycflights13)

df <- flights

## Exercice 1
# Barplot des 10 destinations des plus fréquentés à partir de JFK
# -> tip : summarise(N = n())

data <- df %>% 
  filter(origin == "JFK") %>% 
  group_by(dest) %>% 
  summarise(N = n()) %>% 
  top_n(10)

data %>% 
  ggplot() +
  aes(x = reorder(dest, -N), N) +
  geom_col()

## Exercice 2
# Distribution des vols sur le mois de janvier

df %>% 
  filter(month == 1) %>% 
  ggplot() +
  aes(x = day) +
  geom_bar()


## Exercice 3
# Barplot des 20 destinations les plus déservie par aéroport

data <- df %>% 
  group_by(origin, dest) %>% 
  summarise(N = n()) %>% 
  top_n(20)

data %>% 
  ggplot() +
  aes(x = dest, y = N, fill = dest) +
  geom_col() +
  facet_grid(.~origin, scales = "free")



