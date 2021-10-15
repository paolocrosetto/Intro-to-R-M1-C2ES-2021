                      #III Plotting

#exercice sur la visualisation des données
library(tidyverse)

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")
table(df$dataset)


#calculer la moyenne des datasets
df%>%
  group_by(dataset) %>%
  summarise(mean_x = mean(x),mean_y = mean(y), sd_x=sd(x) , sd_y=sd(y))

#Importance des graphiques pour relayer l'information efficacement et bien visualiser la data
#Présentation des données par ggplot: Les voitures

mpg <- mpg

#aes crée les axes et n'importe quel mapping des données aux éléments graphiques
p<- ggplot(mpg,aes(x=displ,y=hwy))
#opérateur "+" ajouter d'autres éléments au plot
p + geom_point()
p+geom_point()+geom_smooth(method="lm")
p+geom_point()+geom_hline(yintercept = 25,color='red')

##Using AES for other purposes than axes
p+aes(color=class)

#size (taille)
p+aes(color=class, size=cyl)

#tldr:ggplot est comme une grammaire, le 1er argument est le sujet

#BABYNAMES
library(babynames)
babynames
#Evolution du nom "Mary" aux USA
babynames %>% 
  filter(name=="Mary"& sex=="F")-> Maries

#Maries contient l'evolution du nom "Mary" pour les filles

Maries %>% 
  ggplot()+
  #assigner des variables aux axes
  aes(x = year, y = prop)+
  #On ajoute un objet 
  geom_line()

###facets
p
p + facet_wrap(~trans)
#detail de facet

##1. facet_wrap vs facet_grid
p+facet_grid(trans~.)

##exemple avec babynames

# La dynamique deds 2 noms les plus utilises en 1880
# un pour les filles un pour les garcons

babynames %>% 
  filter(year==1880) %>% 
  arrange(-prop)

babynames %>% 
  filter(name == "Mary" | name=="John") %>% 
  ggplot()+
  aes(x = year, y = prop,color=sex)+
  geom_line()+
  facet_wrap(~name)



#Les types de plots
#1 var discrete, mpg$manufacturer

#ordonner les barres

mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N=n()) %>% 
  ggplot() +
  aes(x = reorder(manufacturer,N),fill = drv)+
  geom_bar()

#densité
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_density(adjust=.4)

#scatter
mpg %>% 
  ggplot(aes(x=hwy,y=cty))+
  geom_point()+
  geom_abline(intercept = 0,slope = 1,color = "red", linetype = "dashed")

#PLOT
p<- mpg %>% 
  ggplot()+
  aes(x = class,y = hwy,color = class,fill=class)
p+geom_boxplot()  
p+coord_flip()+geom_boxplot() #mettre a l'horizontale

#raincloud
p+geom_jitter(height=0,width=.2)



###Deux variables, les deux sont discretes
#drv and trans
mpg %>% 
  ggplot()+
  aes(x=class,y=drv)+
  geom_count()
#equivalent à
table(mpg$class,mpg$drv)

###3 Variables
#geom_title
#pour chaque drive, pour chaque N de cylindre, quelle est la consommation moyenne ?
mpg %>% 
  group_by(drv,cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
  ggplot()+
           aes(x = drv,y = cyl, fill= mean_conso)+
  geom_tile()


#Exercice

library(nycflights13)

#Exercice 1
#barplot des 10 destinations les plus fréquentes à partir de JFK
#(tip: use summarise N=n)
df<-flights
data <- df %>% 
  filter(origin=="JFK") %>% 
  group_by(dest) %>% 
  summarise(N=n()) %>% 
  top_n(10)

data %>% 
  ggplot()+
  aes(x=reorder(dest,-N),N)+
  geom_col()


#Exercice2
#Distribution des vols sur le mois de janvier
df
filter(month==1) %>% 
  ggplot()+
  aes(x=day)+
  geom_bar()

#Exercice 3
#Barplot des 20 destinations les plus desservies, par aeroport
data <- df %>% 
  group_by(dest) %>% 
  summarise(N=n()) %>% 
  top_n(20)
data %>% 
  ggplot()+
  aes(x=dest, y=N)+
  geom_col()+
  facet_grid(.~origin, scales="free")

  
