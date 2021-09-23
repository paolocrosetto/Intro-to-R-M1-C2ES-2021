## exercice sur la visualisation des données

library(tidyverse)
df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")
table(df$dataset)

## calcule des moyennes et des écarts-types de chaque dataset

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y), sd_x = sd(x), sd_y = sd(y))

# plotter les dataset

df %>% 
  ggplot(aes(x,y))+
  geom_point()+
  facet_wrap(~dataset)

# GGPLOT
# ggplot = grammar of graphics

mpg <- mpg

# appeler la fonction pour créer un objet plot puis appeler aes en attribuant le nom des axes et des données utilisées
p <- ggplot(mpg, aes(x=displ, y=hwy)) #échelle adaptée par R
summary(mpg)

# opérateur "+" à la place de %>% : ajoute d'autres éléments 
p <- p + geom_point() # ajouter des points
p + geom_smooth() # moyennes conditionnelles sur les intervalles
p + geom_point() + geom_smooth()

# smootther linéaire: MCO
p + geom_point() + geom_smooth(method = "lm")

p + geom_point() + geom_smooth() + geom_hline(yintercept = 25, color = 'red')# ajouter une ligne rouge "plafond" à 25

# utiliser aes pour autre chose que les axes

p + aes(color = class)
p + aes(size = cyl)
p + aes(color = class, size = cyl)
p + aes(shape = fl)

# exercice babynames
library(babynames)

# evolution du prénom "Mary" aux Etats-Unis

# sans plot:
babynames %>% 
  filter(name == "Mary" & sex == "F") -> Maries

# avec plot:
Maries %>% 
  ggplot() + 
  aes(x = year, y = prop)+
  geom_line()

# total:
babynames %>% 
  filter(name == "Jennifer" & sex == "F") %>% 
  ggplot() + 
  aes(x = year, y = prop)+
  geom_line()

## FACETS

p + facet_wrap(~class) # fait un sous-plot pour chaque class
p + facet_wrap(~trans)
p + facet_grid(trans~.) #ligne ~ colonne
p + facet_grid(drv~trans)
# detail de facet:
# facet_wrap : grille unidimentionnelle qui va à la ligne 
# facet_grid : grille bidimentionnelle

# On veut prendre la dynamique des 2 noms les plus utilisés en 1880, un pour les filles et un pour les garçons

babynames %>% 
  filter(year == 1880) %>% 
  arrange(-prop) # c'est John et Mary

babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot() +
  aes(x = year, y = prop, color = sex) +
  geom_line() +
  facet_wrap(~name)

# 1 variable discrète : mpg$manufacturer avec geom_bar() qui est l'équivalent de table()
mpg %>% 
  ggplot() +
  aes(x = manufacturer, fill = drv) + # fill pour remplir de couleur les bar
  geom_bar()

table(mpg$manufacturer)

# ordonner les barres
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N = n()) %>% 
  ggplot() +
  aes(x = reorder(manufacturer, N), fill = drv) +
  geom_bar()

# 1 variable continue : hwy
# histogramme
mpg %>% 
  ggplot() +
  aes(x = hwy) +
  geom_histogram() # bins = 5 : créé 5 bâtons. Sans rien = valeur par défaut

# densité
mpg %>% 
  ggplot() +
  aes(x = hwy) +
  geom_density() # adjust = 0.5 montre une image plus précise de la distribution

# boxplot
mpg %>% 
  ggplot() +
  aes(x = hwy) +
  geom_boxplot()

# violin plot: chacun des violons repésente la densité d'une catégorie
mpg %>% 
  ggplot() +
  aes(x = class, y = hwy) +
  geom_violin()

# 2 variables

# 2 varibales continues

# scatter
mpg %>% 
  ggplot(aes(x = hwy, y = cty)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

# et si tout les points sont au même endroit? On utilise jitter: il décale légèrement la position des données

mpg %>% 
  ggplot(aes(x = hwy, y = cty)) +
  geom_jitter() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

# 2 variables : 1 continue et une discrète : consommation en hwy par class

# plot vide:
p <- mpg %>% 
  ggplot() +
  aes(x = class, y = hwy, color = class, fill = class)

p + geom_boxplot()

# inversons les axes:
p + coord_flip() + geom_boxplot()

p + geom_violin()

# raincould plot

p + geom_jitter(height = 0, width = .2)

# 2 variables discrètes : class et drv

mpg %>% 
  ggplot() +
  aes(x = class, y = drv) +
  geom_count()

# équivaut à 

table(mpg$class, mpg$drv)

# 3 variables

# geom_tile()
# pour chaque drive, pour chaque N de cylindre, quelle est la consommation moyenne?

mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
  ggplot() +
  aes(x = drv, y = cyl, fill = mean_conso) +
  geom_tile()

# exercice

library(nycflights13)
df <- flights

# exo 1 : barplot des 10 destinations les plus fréquentes à partir de JFK
data <-df %>% 
  filter(origin == 'JFK') %>% 
  group_by(dest) %>% 
  summarise(N = n()) %>% 
  top_n(10)

data %>% 
  ggplot() +
  aes(x = reorder(dest, N), N, fill = dest) +
  geom_col()

# exo 2 : distribution des vols sur le mois de janvier
df %>% 
  filter(month == 1) %>% 
  ggplot() +
  aes(x = day, fill = day) +
  geom_bar()

# exo 3 : barplot des 20 destinations des plus deservies par aéroport

data <-df %>% 
  group_by(origin, dest) %>% 
  summarise(N = n()) %>% 
  top_n(20)

data %>% 
  ggplot() +
  aes(x = dest, y = N, fill = dest) +
  geom_col() +
  facet_grid(.~origin, scales = "free") + 
  coord_flip()
