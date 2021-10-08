
library(tidyverse)


df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")


table(df$dataset)


## calcule moyenne et dev st de chaque dataset

df%>%
  group_by(dataset)%>%
  summarize(mean_x=mean(x), mean_y=mean(y),sd_x = sd(x))

##GGPLOT !
## data used for examples
mpg

#appeller la fonction creer un objett plot
ggplot(mpp)

##aes 
p<-ggplot (mpg, aes(x=displ, y = hwy))

#opération "+" ajouter des éléments au plot

p<-p+geom_point()
 
p+geom_smooth()

p+geom_point()+geom_smooth()

# smoother lineaire
p+geom_point()+geom_smooth(method = "lm")

# add reference line
p+geom_hline(yintercept = 25, color= "red")

#using aes for other

# couleur

p +aes(color = class)

# size
p+ aes(size=cyl)

# color+size

p+aes(color = class , size = cyl)

# shape

p+aes(shape=fl)

# exercice babynames

library(babynames)

#evolution du nom "Mary" aux EE UU

# sans plot

babynames %>%
  filter(name == "Jennifer"& sex ==  "F") -> Maries

# "Maries" continet l'évolution du nom " Mary" pour le sfilles

Maries %>%
  ggplot()+
  # assigner des variables aux axzes 
  aes(x= year , y= prop)+
  # on ajoute un object geometrique
  geom_line()

# facets
p
p+facet_wrap(~trans)

# detail de facet 
## facet_wrap vs facet_grip

p+ facet_grid(trans~.)

p+ facet_grid(.~trans)

# grid allows for 2 dimensions
p+ facet_grid(drv~class)

#examples avec babynames"

# la dynamique des 2 noms le splus utilisés en 1880
# un poir le sfilles un poour les garcons *
babynames %>%
   filter(year == 1880)%>%
  arrange(-prop)
# ourrah c'est john  et mary on va avoir un Jésus 

babynames%>%
  filter(name ==  "Mary" | name ==  "John")%>%
  ggplot()+
  aes(x= year, y= prop, color = sex)+
  geom_line()+
  facet_wrap(~name)

###  partons à l'exploration des types de plots !!!

## 1 var, discrète
## mpg$manufacturer 

# geom_bar
# equivalent de table()
 mpg%>%
   ggplot()+
   aes(x=manufacturer, fill = drv)+
   geom_bar()
 
 # ordonner les barres 
 mpg %>%
   group_by(manufacturer)%>%
   mutate(N=n())%>%
   ggplot()+
   aes(x=reorder(manufacturer, N ), fill = drv)+
   geom_bar()

 #   var , continu
 # histogramm --  densité
 ## hwy
 
 mpg %>%
   ggplot()+
   aes(x=hwy)+
   geom_histogram()
 
 # densité
 mpg%>% 
   ggplot()+
   aes(x= hwy)+
   geom_density(adjust =.4) 
 
 # boxplot
 mpg %>%
   ggplot()+
   aes(x=hwy)+
   geom_boxplot()
 
 # violin plot
 mpg %>%
   ggplot()+
   aes(x= class, y=hwy)+
   geom_violin()
 
## 2 variables
 
## 2 continues
 
 # scatter 
 mpg %>%
   ggplot(aes(x = hwy, y = cty))+
   geom_point()+
   geom_abline(intercept = 0,slope = 1, color= "red", linetype= "dashed")
 
 # et si tous les pts sont au meme endroit
 # jitter
 # jitter ajoute du bruit blanvc à la position des données 
 
 
 mpg%>%
   ggplot(aes(x=hwy, y = cty))+
   geom_jitter(width=.3, height=.3)+
                 geom_abline(intercept = 0,slope = 1, color= "red", linetype= "dashed")
 
 
 
 ## 2 var, 1 continur 1 discrète
 # conso en hwy par class
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
 
 ### 2 variables , le sdeux sotn discrètes 
 ### class and drv
 
 mpg %>%
   ggplot()+
   aes(x=class, y= drv)+
   geom_count()
 
 # équivaut à
 table(mpg$drv)
 
 ## 3 variables 
 ## geom_tile
 ## pour chaque drive , pour chaque N de cylindre,
 ## quelle est la consommation moyenne?
 
 mpg %>%
   group_by(drv,cyl)%>%
   summarise(mean_conso = mean(hwy))%>%
   ggplot()+
   aes(x= drv, y= cyl, fill= mean_conso)+
   geom_tile()
 
 
 
 ### Exercices 
 library(nycflights13)
df<- flights 

## exo 1
## barplot des 10 destinations les plus fréquentées à partir de JFK
data<-df%>%
  filter(origin == "JFK")%>%
  group_by(dest)%>%
  summarise(N= n())%>%
  top_n(10)
# base solution 
data %>%
  ggplot()+
  aes(dest, N)+
  geom_col()

# reordering by N 

data %>%
  ggplot()+
  aes(x= reorder(dest, -N), N)+
  geom_col()
## exo 2 
## distributions des vols sur le mois de janvier

df %>%
  filter(month==1)%>%
  ggplot()+
  aes(x= day)+
  geom_bar()
## exo3
## barplot ds 20 dstinations les plus deservies , par aéroport

data<- df %>%
  group_by(origin, dest)%>%
  summarise(N=n())%>%
  
  top_n(20)

data%>%
  ggplot()+
  aes(x=dest, y = N , fill= origin)+
  geom_col()+
  facet_wrap(.~origin, scales= "free")+
  coord_flip()
  
  
