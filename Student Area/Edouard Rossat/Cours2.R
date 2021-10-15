library(tidyverse)
df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

table(df$dataset)

##On calcule moyenne et dev st de chaque dataset

df %>% 
  group_by(dataset) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y),
sd_x = sd(x), sd_y = sd(y))

### GGplot
mpg -> mpg
p <- ggplot(mpg,aes(x = displ, y = hwy))
summary(mpg)
#opérateur "+"
p <- p  + geom_point()
#ajoute des points dans notre graph
#smoother linéaire
#ajoute une ligne de réference reference line
p + geom_smooth()

p + geom_point() + geom_smooth()

p + geom_point() + geom_hline(yintercept = 25, color = 'red') #ajoute une ligne horizontale de col rouge

##using AES for other purposes than axes
#coleur
p + aes(color = class)
p + aes(color = class,size = cyl)

library(babynames) 

babynames %>% 
  filter(name == "Jennifer" & sex == "F") -> Maries

# "maries" contient l'évolution du nom mary pour les filles

Maries %>% 
  ggplot()+
  #assignr des variables aux axes
  aes(x = year, y = prop)+
  #on ajoute un objet geom
  geom_line()

#Facets
p
p + facet_wrap(~trans)

p + facet_grid(.~trans)

#grid allows for 2 dimensions
p + facet_grid(drv~class)

##exemple avec babynames

#la dynamique des 2 noms les plus utilisés en 1880
#un pour les filles un pour les garçons
babynames %>% 
  filter(year == 1880) %>% 
  arrange(-prop)
#ourrah cest john et mary on va avoir un jesus

babynames %>% 
  filter(name == "Mary" | name == "John") %>% 
  ggplot() +
  aes(x = year, y = prop, color = sex)+
  geom_line()+
  facet_wrap(~name)
#deux plots pour les noms john et mary

mpg %>%  
  ggplot() +
  aes(x=manufacturer, fill = drv)+
  geom_bar()

#fill = colore une variable
#ordonner les barres
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N = n()) %>% 
  ggplot()+ 
  aes(x=reorder(manufacturer,N),fill = drv)+
  geom_bar()

##histogram -- densite
##hary
mpg %>% 
  ggplot()+
  aes(x=hwy)+
  geom_histogram()

#densite 
mpg %>% 
  ggplot()+
  aes(x = hwy)+
  geom_density(adjust= 0.4)

##boxplot
mpg %>% 
  ggplot()+
  aes(x = 1,y = hwy) +
  geom_violin()

#scatter : geom point()
mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_jitter()+
  geom_abline(intercept = 0, slope = 1,color = "red", linetype = "dashed")

mpg %>% 
  ggplot(aes(x = hwy, y = cty))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1,color = "red", linetype = "dashed")

##2 var, 1 continue 1 discrete
#conso en hwy par class
#plot vide
p <- mpg %>% 
  ggplot()+
  aes(x = class,y = hwy,color = class,fill = class)
p + geom_boxplot()

#et si on voulait inverser les axes ?

p + coord_flip()  +geom_boxplot()

#violon plot
p + geom_violin()

##raincloud plot
p + geom_jitter(height = 0, width = .2)

### 2 variables, les deux son discrètes

##drv and trans

mpg %>% 
  ggplot()+
  aes(x = class, y = drv)+
  geom_count()

#équivalent à 
table(mpg$class,mpg$drv)

#geon_tile
##pour chaque drive, pour chaque N de cyllindre,
## quelle et la consomation
mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
    ggplot()+
    aes(x = drv, y = cyl,fill = mean_conso)+
    geom_tile()

library(nycflights13)
df <- flights
  
##Exo 1
## barplot des 10 destination les plus fréquentés 
data <- df %>% 
  filter(origin == "JFK") %>% 
  group_by(dest) %>% 
  summarise(N = n()) %>% 
  top_n(10)

#solution de base
data %>% 
  ggplot()+
  aes(dest, N)+
  geom_col()

#reordering by N
data %>% 
  ggplot()+
  aes(x = reorder(dest, -N), N)+
  geom_col()

#Ex 2 
#distribution des vols sur le mois de janvier
df %>% 
filter(month == 1) %>% 
  ggplot() +
  aes(x = day)+
  geom_density()

#exo 3
#barplot des 20 destinations les plus deservies par aeroport 
data <- df %>% 
  group_by(origin,dest) %>% 
  summarise(N = n()) %>% 
  top_n(20)

data %>% 
  ggplot() +
  aes(x = dest, y = N, fill = dest)+
  geom_col()+
  facet_grid(.~origin,scales = "free")+
  coord_flip()
