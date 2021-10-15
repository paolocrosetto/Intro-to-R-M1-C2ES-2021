# CM 3 
# 22/09/2021

#exercice sur la vie des données

library(tidyverse)
df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")
table(df$dataset)


## calcul moyenne et dev st de chaque dataset

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y), sd_x = sd(x), sd_y = sd(y))


### GGPLOT
mpg <- mpg

# appeler la fonction qui creer une objet plot
ggplot(mpg) #creer un plot vide

# aes crée les axes et n'importe quel mapping des données des elements graphiques
p <- ggplot(mpg,  aes(x = displ, y = hwy))

# opérateur "+" : ajouter d'autre element au plot
p <- p + geom_point()
p + geom_smooth()
p + geom_point() + geom_smooth()
p + geom_point() + geom_smooth( method = "lm")

# ajouter lignes de reference
p + geom_point() + geom_hline(yintercept = 25, color = "red")

# using AES for other purposes than axes
# couleur
p + aes( color = class)

#taille
p + aes(size = cyl)

# shape
p + aes (shape = fl)


# EXERCICE babyname
library(babynames)

#evolution du nom "MARY" aux EU

# sans plot
babynames %>% 
  filter(name == "Iris" & sex == "F") %>% 
  ggplot() +
  #assigner des var aux axes
  aes (x = year, y = prop) +
  # on ajoute un obj geometrique
  geom_line()


# Facets
 p + facet_wrap(~class)
 # _wrap est une structure unidimentionnelle qui "va a la ligne"
 # _grid est une grille bidimentionnelle (row column)

 # detail de facet
 # 1. facet_wrap vs facet_grid
 p + facet_grid(trans~.)
 p + facet_grid(.~trans)

# grid allows for 2 dimensions
p + facet_grid(drv~class)


# EXEMPLE  babynames

# la dynamique des 2 noms les plus utilisés en 1880
# un pour les filles et un pour les garcons

babynames %>% 
  filter(year == 1880) %>% 
  arrange(-prop)

babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot() +
  aes(x = year, y = prop, color = sex)+
  geom_line()+
  facet_wrap(~name)


babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot() +
  aes(x = year, y = prop, color = name)+
  geom_line()+
  facet_wrap(~sex)



# exploration des types de plots

# 1. var discrete
#msg$manufacturer

mpg %>% 
  ggplot()+
  aes(x = manufacturer, fill = drv) +
  geom_bar()  # equivalent de table()

# geom_bar prend un element important qui est la position
# ordonner les barres
# par defaut geom_bar fait par ordre alphabetique

mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N = n()) %>% 
  ggplot()+
  aes(x = reorder(manufacturer, -N), fill = drv) +
  geom_bar() 


# une variable continue
# histogramme
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_histogram()  #bins

# densité
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_density()  # adjust

#boxplot
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_boxplot()

#violin plot
mpg %>% 
  ggplot()+
  aes(x = class, y = hwy)+
  geom_violin()


## deux variables continues

#scatter : avec geom_point
mpg %>% 
  ggplot(aes(x = hwy , y = cty))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red", linetype = "dashed")

# et si tous les points sont au mm endroit?
#jitter : ajouter un peu de mvt aleatoire/ du bruit blanc a la postion des données : on fait ressortir les points sont cachés les uns par les autres
mpg %>% 
  ggplot(aes(x = hwy , y = cty))+
  geom_jitter(width = .2, height= .2)+
  geom_abline(intercept = 0, slope = 1, color="red", linetype = "dashed")

# deux variables : 1 continue une discrete
# conso en hwy par class

# plot vide
p <- mpg %>% 
  ggplot()+
  aes (x = class, y = hwy, color = class, fill=class)
p + geom_boxplot()
# si on veut inverser les axes
p + coord_flip() + geom_boxplot()

#violnn plot
p+geom_violin()

# raincloud plot
p + geom_jitter(height = 0, width = .2)

# 2 varibles discretes
# class et drv
mpg %>% 
  ggplot()+
  aes(x = class, y =drv)+
  geom_count()

# 3 variables
#geom_tile
# pour chaque drive, pour chaque N de cylindre, quel est la conso moyenne?

mpg %>% 
  group_by(drv,cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
  ggplot()+
  aes(x = drv, y = cyl, fill = mean_conso)+
  geom_tile()


# EXERCICE
library(nycflights13)
df <- flights

# ex 1 : barplot des 10 destinations les plus frequentes a partir de JFK
# ex 2 : distribution des vols sur le mois de janvier
# ex 3 : barplot des 20 destinations les plus desservie par aeroport

data <- df %>% 
  filter( origin == "JFK") %>% 
  group_by(dest) %>% 
  summarise(N = n()) %>% ## tally()
  top_n(10)

data %>%
  ggplot()+
  aes(x = dest, y = N)+
  geom_col()

data %>% 
  ggplot()+
  aes(x = reorder(dest, -N), N)+
  geom_col()


# ex 2 : distribution des vols sur le mois de janvier

df %>% 
  filter(month == 1) %>% 
  ggplot()+
  aes(x = day)+
  geom_bar()
  

# ex 3 : barplot des 20 destinations les plus desservie par aeroport
data <- df %>% 
  group_by(origin, dest) %>% 
  summarise(N = n()) %>% 
  top_n(20)

data %>% 
  ggplot()+
  aes(x = dest, y = N, fill= dest)+
  geom_col()+
  facet_wrap(. ~origin, scales = "free") +
  coord_flip()
  