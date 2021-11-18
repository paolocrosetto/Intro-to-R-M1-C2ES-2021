### note L3 Plotting
## exercice sur la visualisations des   données

library(tidyverse)
df <- read_tsv("lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")
table(df$dataset)
# calcule des moyenne et écart-type de chaque dataset

 df %>% 
  group_by(dataset) %>% 
summarise(mean_x = mean(x), mean_y = mean(y), sd_x=sd(x),sd_y=sd(y))
#ploter les dataset

df %>%
  ggplot(aes(x,y))+
  geom_point()+
  facet_wrap(~dataset)
  
  
### data used for example
#ggplot = grammar of graphics
mpg<- mpg
## appeler la fonction pour  créer un objet plot puis appeler AES en attribut le nom des axes et des données utilisé 
ggplot(mpg)


# AES crée les axes et n'import quel mapping des données aux élements graph
p <- ggplot(mpg,aes(x = displ,y = hwy))

#opération"+": ajouter d'autres élément au plot
p <- p + geom_point()

p + geom_smooth()

p + geom_point() + geom_smooth()

#smoother linéaire : MCO
p + geom_point() + geom_smooth(method = "lm")

# add reference line ==> ajouter une ligne de réference rouge plafonée à 25
p + geom_point() + geom_hline(yintercept = 25, color = 'red')

# using AES for other purpose than axes ==> utilier AES pour autre chose que les axes
    #----couleur
p + aes(color = class) 
   #----size ( taille)
p + aes(size = cyl)
  #-----couleur and size
p + aes(color = class,size = cyl)

 # shape
p +aes(shap=fl)



#---------------------------EXERCICE SUR BABYNAMES -------------------------------
library(babynames)


               #1.evolution du nom "marie aux USA
                    #--- SANS PLOT
babynames %>%
  filter(name== "Mary" & sex == "F") -> maries

                # AVEC PLOT
maries %>%
   ggplot()+
  #assigner des varibles aux axes
  aes(x= year,y=prop)+
  #non ajouteum objet géometrique
  geom_line()
#-----------------------------------------------------------------------------

##  FACETS
p
p + facet_wrap(~class) # fait un sous-plot pour chaque class

# wrap est une structure muldimentionnelle qui va à la ligne
# grid est une structure muldimentionnelle (row colums)
# detail de facet 

#1   facet_x=wrap vs facet_grid
p + facet_grid(.~trans)

 #-------------------- EXEMPLE : avec BABYNAMES   ----------------
   # la dynamique des 2 noms les plus utilisés en 1880
   # un pour les filles un pour les garçons
view(babynames)
babynames %>%
  filter(year == 1880) %>%
  arrange(-prop)
# mary and john

babynames %>%
  filter(name == "Mary" | name == "John") %>%
  ggplot()+
  aes(x=year,y = prop,color = sex)+
  geom_line() +
  facet_wrap(~name)
  
### partons à l'exploration des types de plots!!!

    # 1 variable discrete : mpg$manufacturer avec geom_bar() qui équivalent de table()
  
mpg %>%
  ggplot()+
  aes(x=manufacturer,fill = drv)+
  geom_bar()

  #ordonner les barres

mpg %>%
  group_by(manufacturer)  %>%
  mutate(N = n()) %>%
  ggplot()+
  aes(x=reorder(manufacturer,N),fill = drv)+
  geom_bar()

   # 1 variable continue
      # histogramme 
mpg %>%
  ggplot()+
  aes(x=hwy)+
  geom_histogram()

     # densité
mpg %>%
  ggplot()+
  aes(x=hwy)+
  geom_density(adjust = .4)

   #boxplot
mpg %>%
  ggplot()+
  aes(x=hwy)+
  geom_boxplot()

   #violin plot
mpg %>%
  ggplot()+
  aes(x=1,y=hwy)+ # avec une varibale
  geom_violin()

mpg %>%
  ggplot()+
  aes(x=class,y=hwy)+
  geom_violin()


## 2 variables continues
   # scatter
mpg %>%
  ggplot(aes(x=hwy,y=cty))+
  geom_point()+
  geom_abline(intercept = 0,slope = 1, linetype = "dashed")

 # et si tous les points sont au mm endroit ? on utilise jitter
 # jitter décale légérement la position des données
 # jitter ajoute du bruit blanc à la postion des données
mpg %>%
  
  ggplot(aes(x=hwy,y=cty))+
  geom_jitter(width = .3,height = .3)+
  geom_abline(intercept = 0,slope = 1, color = "red",linetype = "dashed")


# 2 varibles : 1 continue et  1 discret 
              # =====> consommation en hwy par class
#plot vide
p <- mpg %>%
  ggplot()+
  aes(x = class,y =hwy,color= class,fill= class)
p + geom_boxplot()

# et si on voulait inverser les axes?
p + coord_flip()+geom_boxplot()
# VIOLIN PLOT
p + geom_violin()
#raincloud plot
p + geom_jitter(height = 0,width = 2 )
#####2 variblz ,les 2 discrets
##class and drv
 #drv and trans

mpg %>%
  ggplot()+
  aes(x = class,y =drv)+
  geom_count()
## equivalent à
table(mpg$class,mpg$drv)

## 3 variables
#  GEOM_TILE
# pour chaque drive,pour chaque N de cylindre,quelle est la consommation moyenne?
mpg  %>%
  group_by(drv,cyl) %>%
  summarise(mean_conso = mean(why))  %>%
  ggplot()+
  aes(x = drv,y = cyl,fill = mean_conso)
  geom_tile
  
#----------------------------------EXERCICES sur flights-------------------------

library(nycflights13)
df <- flights
view(df)
  #Exo1
  #barplot des 10 destinations les plus fréquentes à partir de JFK
data <- df %>%
   filter(origin == "JFK") %>% 
   group_by(dest) %>%
   summarise(N = n()) %>%
   top_n(10)
    
   
   #base solution
data %>%
  ggplot() +
  aes(dest, N)+
  geom_col()

   #reording by N 
data %>%
  ggplot() +
  aes(x= reorder(dest, -N),N)+
  geom_col()

  

  
  
  
  #exo2 : distributions des vols sur le mois de janvier
df %>%
  filter(month == 1)%>%
  ggplot() +
  aes(x=day, fill=day)+
  geom_bar()
  
  
  
  #exo3: barplot des 20 destinations les plus desservie par aeroport
data <- df %>%
  group_by(origin,dest)%>%
  summarise(N = n()) %>%
  top_n(20)

data %>% 
  ggplot() +
  aes(x = dest, y = N, fill = dest) +
  geom_col() +
  facet_grid(.~origin, scales = "free") + 
  coord_flip() 



      