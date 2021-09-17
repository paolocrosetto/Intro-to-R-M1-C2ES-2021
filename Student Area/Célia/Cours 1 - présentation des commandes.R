library(tidyverse) #rend disponible le package "tidyverse"

objet <- 5  #on cree objet qui contient 5

objet**2 #objet au carre

objet**2 -> objet2  #met objet au carre dans objet2 (objet2=objet^2)

?hist #affiche l'explication de la commande hist dans help

a <- c(1, 2, 3) #concatenate : cree un vecteur 1 2 3
b <- c("A","B","C") 
ab <- c(a, b) #On ne peut pas creer un vecteur avec des types de caracteres differents mais on peut faire plusieurs vecteurs puis les concatener

c <- c(4, 5, 6)

rbind(a, c) #colle en ligne pour faire une matrice
cbind(a, c) #colle en colonne pour faire une matrice

df <- airquality #data frame : on met toujours les donnees dans df pour que ce soit moins long a recopier
view(df)  #affiche df sous forme de tableau
df$Ozone #n'affiche que la colonne Ozone
plot(df) #affiche le graphique a partir des donnees de df
df$Month[1] #les crochets permettent d'aller a un endroit precis
df$Month[1:5]

mpg <- mpg #jeu de donnees qu'on va utiliser pour travailler
view(mpg) 
summary(mpg) #donne des infos sur mpg comme la taille, valeur min et max, moyenne, etc.


