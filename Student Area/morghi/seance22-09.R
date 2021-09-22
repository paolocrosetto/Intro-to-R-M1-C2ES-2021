##notes L3 plotting
library(tidyverse)

df <- read_tsv("Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv")

table(df$dataset)

#calcule moyenne et dev st de chaque dataset

df %>%
  group_by(dataset) %>%
  summarise(mean_x = mean(x),mean_y = mean(y),sd_x= sd(x), sd_y = sd(y))

##plotter les dataset
 df %>%
   ggplot(aes(x,y))+
   geom_point()+
   facet_wrap(~dataset)
##GGPLOT 
 mpg <- mpg
 
#appeler la fonction crée u  objet plot
 ggplot(mpg)
 #aes cree les axes et n'importe quel mapping des données aux elements graphes 
 p <- ggplot(mpg, aes(x= displ , y= hwy))
 
 #opérateur "+" : ajouter des elements au plot 
 p + geom_point()
 
 p + geom_smooth()
 
 #add reference line
 P <- p + geom_point() + geom_hline(yintercept = 25 ,color='red')

 ###using aes other purpose than axes 
 
 # color
 P <- P + aes(color = class)
 #size taille 
 P <- P + aes (size = cyl)
 
 #both
  P <- P +aes(color = class , size =cyl)
  
  #exercice baybynames
  library(babynames)
#evolution du nom Marry au USA
  
  #sans plot
  babynames %>%
    filter(name == "Mary" & sex == "F") -> Maries 
  #maries contient l'evolution du nom mary pour les filles 
   Maries %>%
     ggplot()+
     #assigner des variables aux axes 
     aes(x = year , y = prop)+
     #on ajoute objet geometrique
     geom_line()

## facets 
  P + facet_wrap(~drv)
  P + facet_wrap(~class)
# detail de facet 
  ## 1.facet_wrap vs facet_grid
  P + facet_grid(trans~.)
  P + facet_grid(.~trans)  
  # grid allows for 2 dimensions 
  P + facet_grid(drv~class)
# exemple avec babynames 
  #la dynamique des 2 noms les plus utilisés en 1880
  # un pour les filles et un pour les garçons
  
  babynames %>%
    filter(year == 1880 ) %>%
    arrange(- prop)
  ## 
  babynames %>%
    filter(name == "Mary" | name == "John") %>%
    ggplot()+
    aes(x = year , y = prop ,color = sex)+
    geom_line()+
    facet_wrap(~name)
  
###variable discrete 
##mpg$manufacteur
  mpg %>%
    ggplot()+
    aes(x = manufacturer,fill = manufacturer)+
    geom_bar() # ==equivalent du table()
  
  table(mpg$manufacturer)
  
  ## ordonner les barres
  
  mpg %>%
    group_by(manufacturer) %>%
    mutate(N = n()) %>%
    ggplot()+
    aes(x = reorder(manufacturer,N),fill = drv)+
    geom_bar()
  
  # 1 var continue 
  ##histogram  densite 
  #hwy
  
    mpg %>%
    ggplot()+
    aes(x=hwy)+
    geom_histogram()
 
  #densite
    
    mpg %>%
      ggplot()+
      aes(x=hwy)+
      geom_density(adjust = 5)
  #boxplot
    mpg %>%
      ggplot()+
      aes(x=hwy)+
      geom_boxplot()
  #violin plot 
    mpg %>%
      ggplot()+
      aes(x=1 , y = hwy)+
      geom_violin()
  ### deux variables
    ## 2 continues 
    
    #scatter :geom_point
    mpg %>%
      ggplot(aes(x= hwy,y = cty))+
      geom_point()+
      geom_abline(intercept = 0,slope =1, color = "red",linetype= "dashed")
    
    #et si touts les points sont au meme endroit 
    #jitter
    #jitter ajouter du bruit blanc à la position des données 
    mpg %>%
      ggplot(aes(x= hwy,y = cty))+
      geom_jitter(width =.3,height = .3) +
      geom_abline(intercept = 0,slope =1, color = "red",linetype= "dashed")
    
   ## 2 var 1 cont 1 dis 
    #conso en hwy par class
    
    
    #plot vide
    p <- mpg %>%
      ggplot()+
      aes(x =class ,y =hwy,color =class ,fill = class)
    
    p+ geom_boxplot()
 #et si on voulait inverser les axes 
    p+ coord_flip() + geom_boxplot()
## violin plot 
    p + geom_violin()

    #raincloud plot le mieux que boxplot et violin 
    p + geom_jitter(height = 0,width = .2)

    ## 2 var les deux discretes
     # class and trans
    
    mpg %>%
      ggplot()+
      aes(x= class ,y =drv)+
      geom_count()
    #équivalent à
    table(mpg$class,mpg$drv)
   
    ### 3 variables 
    #geom_tile
    ##pour chaque drive pour chaque N de cylindre 
    ## quelle est la consommation moyenne
    
     mpg %>%
       group_by(drv,cyl)  %>%
       summarise(mean_cons= mean(hwy)) %>%
     ggplot()+
       aes(x=drv , y =cyl ,fill =mean_cons)+
       geom_tile()

  #Exercices
     library(nycflights13)
     df <- flights
    names(df)
    ##barplot des 10 destinations les plus frequentes à partir de JFK
       df %>%
         filter(origin == "JFK") %>%
         group_by(dest) %>%
         summarise(N= n())
        top_n(10)
       