## Cours 3 (lecture 3)
# on va faire des graphiques 
## ploter les donner c'est important pour voir ce que ca représente
## An example : the DatasaurusDozen 
# exercice sur la visualisation des données

library(tidyverse)

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

table(df$dataset)

## calcule moyenne et dev st de chaque dataset

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y),
            sd_x=sd(x), sd_y=sd(y))

## la position est la voix de l'information, la couleur, taille de l'objet
## c'est super facile de montrer ce que l'on veut sur un graphique, on peut le biaiser
## le camembert c'est pas fou car les angles c'est compliqué à différencier 
## on va donc faire du good plotting, pour ça il faut 
# ---- connaitre ses variables
# ---- utiliser un plot adéquate pour le message que l'on veut passer
# ---- on peut dessiner pour visualiser
# ---- je choisis les axes (y et x)
# ---- je choisis le type de visualisation 
# ---- je suis honnete et je n'enleve pas des données
# ---- je cherche à mettre le moins d'informations

### pour faire des plots on utlise "ggplot" et un data "mpg"
## ggplot -> grammar of graphics
## La grammaire des graphiques j'ai besoin :
# des données 
# des objects géométriques 
# outils stats 
# d'une échelle
# d'un système de coordonnées 
# d'ajustement des positions 

## data used for examples :

mpg <- mpg

## ggplot(mpg) -> cela va créer un plot vide 
ggplot(mpg)

## p <- ggplot(mpg, aes(x = displ, y = hwy))
# ici aes permet de tracer les axes 
# ici c'est mpg -> sujet et aes -> verbe

p <- ggplot(mpg, aes(x = displ, y = hwy))

# opérateur "+" (permet d'ajouter des couches/ d'autres éléments)

p <- p + geom_point()

p + geom_smooth()
# smooth -> tendance (pour la tracer)

p + geom_point() + geom_smooth()

p + geom_point() + geom_smooth(method="lm")
# smooth linéaire (comme en économetrie)

p + geom_point() + geom_hline(yintercept=25, color='red')
# add reference line 

### Using aes for other purposes than axes

p + aes(color = class)
p + aes(color = drv)
# color avec tout ce qu'on veut 

p + aes(size = cyl)
#size (taille)

p + aes(size = cyl, color = class)
# la combinaison des deux



p + aes(shape=fl)
# shape

## Comme en grammaire :
# on commence par ggplot
# puis on met df : sujet : ggplot(df,..)
# puis les variables aes.. (voir cours)


## exercice babynames 
library(babynames)
babynames

## evolution du nom 'Mary' aux EU

## sans plot
babynames %>% 
  filter(name == "Mary" & sex == "F") -> Maries

# Maries conteint l'évolution du nom Mary pour les filles 

Maries %>% 
  ggplot()+
  #assigner des variables aux axes
  aes(x = year, y = prop)+
  # on ajoute objet géométrique
  geom_line()

### facets

p + facet_wrap(~trans)
## _wrap est une structure unidimentionnelle qui "va à la ligne"
## _grid est une grille bdimentionnelle (row-column)

## detail de facet

## 1. facet_wrap vs facet_grid
p + facet_grid(trans~.)

p + facet_grid(.~trans)

## grid allows for 2 dimensions 

p + facet_grid(drv~class)

## example avec babynames

# la dynamique des 2 noms les plus utilisés en 1880
# un pour les filles un pour les garcons 

babynames %>% 
  filter(year == 1880) %>%
  arrange(-prop)

## ourrah c'est John et Mary on va avoir un Jesus 

babynames %>% 
  filter(name == "Mary" |name == "John") %>% 
  ggplot()+
  aes(x=year, y=prop, color = sex)+
  geom_line()+
  facet_wrap(~name)

## Plus de détails sur la grammaire 
# data(sujet)
#axis(verbe)
#geoms(objet)
#...... voir le diapo


### exploring data with plots : one variable (partons à l'exploration des types de plots)

## 1 variable discrete
## mpg$manufacturer

mpg %>% 
  ggplot()+
  aes(x=manufacturer, fill = drv)+
  geom_bar()

## si on veut remplir on ne dit pas color mais fill

# ordonner les barres (reorder)

mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N=n()) %>% 
  ggplot()+
  aes(x=reorder(manufacturer,N), fill = drv)+
  geom_bar()


## 1 var continue

## histogram -- densité
## hwy
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_histogram()

## densité 
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_density(adjust=.4)

## boxplot (boite à moustache)
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_boxplot()

## violin plot 
mpg %>% 
  ggplot()+
  aes(x=class, y= hwy)+
  geom_violin()

mpg %>% 
  ggplot()+
  aes(x=1, y= hwy)+
  geom_violin()

### 2 variables 

## 2 var continues 

# scratter
mpg %>% 
  ggplot(aes(x=hwy, y=cty))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color ="red", linetype = "dashed")
## abline -> ligne a + bx (intercept = a, slope la pente b)


# et si tout les points sont au même endroit ? 
# on va alors utilsier jitter (on ajoute un petit mouvement aléatoire (bruit blanc à la position des données))

mpg %>% 
  ggplot(aes(x=hwy, y=cty))+
  geom_jitter()+
  geom_abline(intercept = 0, slope = 1, color ="red", linetype = "dashed")

mpg %>% 
  ggplot(aes(x=hwy, y=cty))+
  geom_jitter(width = 100, height= 0.4)+
  geom_abline(intercept = 0, slope = 1, color ="red", linetype = "dashed")
## c'est bien d'utilsier jitter mais quand c'est trop ça peut rendre nos données fausses


## 2 var, 1 continu et 1 discrete
# conso en hwy par class

# plot vide
p <- mpg %>% 
  ggplot()+
  aes(x=class,y=hwy, color=class, fill=class)

p + geom_boxplot()


# et si on voulait inverser les axes ? 
p + coord_flip() + geom_boxplot()

# violin plot
p + geom_violin()

## raincloud plot 
p + geom_jitter(height=0, width =.2)
## on va ajouter jitter horizontale sans toucher les données (sauf sur le graph mais c'est hozitontal donc change pas les valeurs)

### 2 variables, les deux sont discretes 
## drv and class

mpg %>% 
  ggplot()+
  aes(x=class,y=drv)+
  geom_count()

## equivalent à
table(mpg$class, mpg$drv)


### 3 variables 

## geom_tile
## pour chaque drive, pour chaque N de cylindre, quelle est la consommation moyenne ? 

mpg %>% 
  group_by(drv,cyl) %>% 
  summarise(mean_conso=mean(hwy)) %>% 
  ggplot()+
  aes(x=drv, y=cyl, fill = mean_conso)+
  geom_tile()


#### Exercices 

library(nycflights13)

df <- flights

## exo 1 
## barplot des 10 destinations les plus fréquentes à partir de JFK

View(df)

data <- df %>% 
  filter(origin=="JFK") %>% 
  group_by(dest) %>% 
  summarise(N=n()) %>% ###tally()
  top_n(10)

data %>% 
  ggplot()+
  aes(dest,N)+
  geom_col() ###il compte pour nous

data %>% 
  ggplot()+
  aes(x=reorder(dest,-N),N)+
  geom_col()


## exo 2 
## distribution des vols sur le mois de janvier 


df %>% 
  filter(month==1) %>% 
  ggplot()+
  aes(x=day)+
  geom_bar() ###ici en barres

df %>% 
  filter(month==1) %>% 
  ggplot()+
  aes(x=day)+
  geom_density() ### ici la densité 

## exo 3 
## barplot des 20 destinations les plus desservies, par aéroport  


##### ICI C'EST FAUXXXXXXX
data <- df %>% 
  group_by(dest,origin) %>% 
  summarise(N=n()) %>% 
  top_n(20)

data %>% 
  ggplot()+
  aes(x= dest, y = N, fill = dest)+
  geom_col()+
  facet_grid(.~origin,scales="free")+
  coord_flip()


  
