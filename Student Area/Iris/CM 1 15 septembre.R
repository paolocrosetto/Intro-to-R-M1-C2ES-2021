
library(tidyverse)
library(tidyverse)
objet <- 5
airquality
plot(airquality)
hist(airquality)
view(airquality)
mean(airquality$Wind)
df <- airquality #assigner airquality dans l'environnement#

#vecteur
c(1,2,3) # un vecteur
a <- c(1,2,3)  #vecteur numerique
b<- c("A","B") #vecteur caractere
ab <-c(1,2,"A")  #chaine charactere plus puissant que numérique ; concatenate

#matrice
r1 <- a
r2 <- c(4,5,6)
rbind(r1,r2)  #regroupe r1 et r2 en une matrice et par ligne 
rbind(r1,r2) -> m1
m1
View(m1)

airquality
airquality$Wind # ca me montre la colone Wind ; $ montre un element précis ; matrix extraction
# [] extraction de le matrice

mpg
mpg<-mpg
View(mpg)
