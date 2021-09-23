### notes L3 plotting
##exercice sur la vie de données
library(tidyverse)
df<-read_tsv("lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

table(df$dataset)

##calcule moyenne et dev st de chaque dataset
df %>% 
  group_by(dataset) %>% 
summarise(mean_x=mean(x), mean_y=mean(y),
          sd_x=sd(x), sd_y=sd(y))

### GGPLOT !
## data used for examples
mpg<-mpg

#appeler la fonction crée un objet plot
ggplot(mpg) #cree un plot vide

#in grammar you need a verb, dans les plot c'est les axes 


#aes crée les axes et nimporte quel mapping des données aux elements graphiques
p<-ggplot(mpg, aes(x=displ, y=hwy)) #R choisi automatiquement les echelles

summary(mpg) #au cas où on veut changer les echelles apres

#operateur "+" : permet d'ajouter des éléments au plot
p<-p + geom_point()

p + geom_smooth()

p + geom_smooth() + geom_point()

#smoother lineaire
p + geom_point() + geom_smooth(method = "lm")

#add reference line
p + geom_point() + geom_hline(yintercept = 25 , color = "red")

## using AES for other purposes than exes
#couleur
p+aes(color=class)

#size(taille)
p + aes(size=cyl)

#couleur and size
p + aes(color=class, size=cyl)

#shape
p + aes(shape=fl)

## exercice babynames
library(babynames)

#evolution du nom Mary aux EEUU

#Sans plot
babynames %>% 
  filter(name== "Mary", sex== "F") %>% 
  ggplot()+
  #assigner des variables aux axes
  aes(x=year, y= prop)+
  #on ajoute un objet geometrique
  geom_line()

###facets
p
p + facet_wrap(~class)
# wrap zst une structure unidimensionnelle qui va à la ligne
# _grid est une grille bidimensionnelle( ligne + colonne)
## detail de facet

## 1. facet_wrap vs facet_grip

p + facet_grid(trans~.)

p + facet_grid(drv~class)


#exemple avec babynames

# la dynamique des 2 noms les plus utilisés en 1880
#un pour les filles et un pour les garçons

babynames %>% 
  filter(year== 1880) %>% 
  arrange(-prop)
# c'est john et mary

babynames %>% 
  filter(name== "Mary" | name== "John") %>% 
  ggplot()+
  aes(x=year, y = prop, color=sex)+
  geom_line()+
  facet_wrap(~name)# permet d'avoir un plot pour john et un autre pour Mary 


### partons à l'exploration des types de plots

## 1 var , discrete 
# mpg$manufacturer

#geom_bar
#equivalent de table()
mpg %>% 
  ggplot()+
  aes(x=manufacturer, fill= manufacturer)+
  geom_bar()

#
mpg %>% 
  ggplot()+
  aes(x=manufacturer, fill= drv)+
  geom_bar()
#ordonner les barres
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N=n()) %>% 
  ggplot()+
  aes(x=reorder(manufacturer, -N ), fill= drv)+
  geom_bar()

## 1 var continue
## histogramme -- densité
## hwy
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_histogram()

## densité
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_density(adjust = 5)

##☺ boxplot
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_boxplot(adjust = 5)


## violin plot
mpg %>% 
  ggplot()+
  aes(x=class, y=hwy)+
  geom_violin()


### deux variables
## deux sont continues

# scatter
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_point()+
  geom_abline(intercept = 0 , slope= 1, color = "red", linetype = "dashed")


# et si tous les points sont au meme endroit 
#jitter
# jitter ajoute du bruit blanc à la position des données
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_jitter(width = .1, height = .1)+
  geom_abline(intercept = 0 , slope= 1, color = "red", linetype = "dashed")

## 2 variable, 1 continue et une discrete 
#conso en hwy par class

#plot vide
p<- mpg %>% 
  ggplot()+
  aes(x=class, y=hwy, color = class, fill = class)
p

p + geom_boxplot()

# et si on voulait inverser les axes
p + coord_flip() + geom_boxplot()

#violin plot
p + geom_violin()


# raincloud plot
p + geom_jitter(height = 0, width = .2)

### 2 variables, les deux sont discretes

## drv and trans
mpg %>% 
  ggplot()+
  aes(x = class, y = drv)+
  geom_count()


## equivalent à 
table(mpg$class, mpg$drv)

###3 variables

## geom_tile

## pour chaque drive, pour chaque N de cylindre, quelle est la consommation moyenne 

mpg %>% 
  group_by(drv,cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
    ggplot()+
    aes(x = drv, y = cyl, fill = mean_conso)+
    geom_tile()




##### Exercices 
library(nycflights13)
df<- flights


## exo1 
## barplot des 10 destinations les plus frequentes à partir de JFK
## tip: use summarise(N, n())
data <- df %>% 
  filter(origin == "JFK")%>% 
  group_by(dest) %>% 
  summarise(N = n()) %>% 
  top_n(10)


data %>% 
  ggplot()+
    aes(dest, N)+
  geom_col()
  

data %>% 
  ggplot()+
  aes(x = reorder(dest, -N), N)+
  geom_col()


## exo2 
## distributions des vols sur le mois de janvier 
df %>% 
  filter(month == 1) %>% 
  ggplot()+
  aes(x = day)+
  geom_bar()


## exo3
## barplot des 20 destinations les plus desservie par aeroport
data<-df %>% 
  group_by(origin, dest) %>% 
  summarise(N = n()) %>% 
  top_n(20)
data %>% 
  ggplot()+
  aes(x = dest , y = N, fill= dest)+
  geom_col()+
  facet_grid(.~origin, scales = "free")
