# CONTRÔLE
library(tidyverse)
library(lubridate)

scoobydoo <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

summary(scoobydoo)

scoobydoo <- scoobydoo %>%
  mutate(genre = case_when(
    str_detect(monster_gender, "Male") & str_detect(monster_gender,"Female") ~ "Mixte",
    str_detect(monster_gender, "Male") ~ "Masculin",
    str_detect(monster_gender, "Female") ~ "Féminin",
    TRUE ~ "Pas de Genre")) %>% 
  mutate(year = year(date_aired)) %>% 
  mutate(decennie = floor(year(scoobydoo$date_aired)/10)*10) %>%  #floor(year(scoobydoo$date_aired)/10)*10
  mutate(imdb2 = as.numeric(imdb)) %>% 
  mutate(motive2 = case_when(
    motive == "Abduction" ~ "Abduction", 
    motive == "Competition" ~ "Competition",
    motive == "Conquer" ~ "Conquer",
    motive == "Natural Resource" ~ "Natural Resource",
    motive == "Preservation" ~ "Preservation",
    motive == "Smuggling" ~ "Smuggling",
    motive == "Theft" ~ "Theft",
    motive == "Treasure" ~ "Treasure",
    is.na(motive)  ~ "None",
    TRUE ~ "Other"))


# nb d'épisodes répertoriés
dim(scoobydoo)[1]

# noms des différents types de formats
table(scoobydoo$format)[1]

scoobydoo %>% 
  ggplot() +
  aes(x="", y = format, fill = format) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "répartition")

scoobydoo %>% 
  ggplot() +
  aes(x=format, fill = format) +
  geom_bar()



captuT <- cbind(scoobydoo$captured_fred,scoobydoo$captured_daphnie,scoobydoo$caught_velma,
                scoobydoo$captured_scooby, scoobydoo$captured_shaggy)



n <- ncol(captuT)
for(i in 1:n){
  captuT[,i] <- str_replace(captuT[,i],"TRUE", "1")
  captuT[,i] <- str_replace(captuT[,i],"FALSE", "0")
  captuT[,i] <- str_replace(captuT[,i],"NULL", "0")
}
  

captuT <- data.frame(captuT)
colnames(captuT) <- c("Fred","Daphnie","Velma","Scooby","Shaggy")
plot(x=NA, y=NA, xlim=c(1,603), ylim=c(0,165), xlab = "?pisode", ylab = "capture")
nr <- nrow(captuT)






for (i in unique(CF) ){
  print( cumsum(CF[i]) )
}






z=0
for (i in 1:n)
  for(j in 1:nr){
    lines(x = c(1:603), y = z + as.numeric(captuT[j,i]))
    z = y
  }

plot(x=NA, y=NA, xlim=c(0,603), ylim=c(0,1), xlab = "?pisode", ylab = "capture")
z=0    
for (i in 1:n){
  lines(x = c(1:603), y = z + as.numeric(captuT[,i]))
  z = y
}
  




CF <- data.frame(scoobydoo$captured_fred)
CF
n <- ncol(CF)
for(i in 1:n){
  CF[,i] <- str_replace(CF[,i],"TRUE", "1")
  CF[,i] <- str_replace(CF[,i],"FALSE", "0")
  CF[,i] <- str_replace(CF[,i],"NULL", "0")
}
CF <- as.numeric(CF[,i])
CF <- data.frame(CF)
CF

z=0
for(i in 2:n){
  CF[i,1] <- z + sum(CF[i,1], CF[(i-1),1])
  z = z + sum(CF[i,1], CF[(i-1),1])
}

CF[2,1] <- sum(CF[2,1], CF[1,1])

1 + sum(1,2)


########################################
##fred
CF <- data.frame(scoobydoo$captured_fred)
CF
n <- ncol(CF)
for(i in 1:n){
  CF[,i] <- str_replace(CF[,i],"TRUE", "1")
  CF[,i] <- str_replace(CF[,i],"FALSE", "0")
  CF[,i] <- str_replace(CF[,i],"NULL", "0")
}
CF <- as.numeric(CF[,i])
CF <- data.frame(CF)
CF
for (i in unique(CF) ){
  print( cumsum(CF[i]) )
}
##daphnie
CD <- data.frame(scoobydoo$captured_daphnie)
CD
n <- ncol(CD)
for(i in 1:n){
  CD[,i] <- str_replace(CD[,i],"TRUE", "1")
  CD[,i] <- str_replace(CD[,i],"FALSE", "0")
  CD[,i] <- str_replace(CD[,i],"NULL", "0")
}
CD <- as.numeric(CD[,i])
CD <- data.frame(CD)
CD
for (i in unique(CD) ){
  print( cumsum(CD[i]) )
}
##velma
CV <- data.frame(scoobydoo$captured_velma)
n <- ncol(CV)
for(i in 1:n){
  CV[,i] <- str_replace(CV[,i],"TRUE", "1")
  CV[,i] <- str_replace(CV[,i],"FALSE", "0")
  CV[,i] <- str_replace(CV[,i],"NULL", "0")
}
CV <- as.numeric(CV[,i])
CV <- data.frame(CV)
for (i in unique(CV) ){
  print( cumsum(CV[i]) )
}
##scooby
CSO <- data.frame(scoobydoo$captured_scooby)
n <- ncol(CSO)
for(i in 1:n){
  CSO[,i] <- str_replace(CSO[,i],"TRUE", "1")
  CSO[,i] <- str_replace(CSO[,i],"FALSE", "0")
  CSO[,i] <- str_replace(CSO[,i],"NULL", "0")
}
CSO <- as.numeric(CSO[,i])
CSO <- data.frame(CSO)
for (i in unique(CSO) ){
  print( cumsum(CSO[i]) )
}
##shaggy
CSH <- data.frame(scoobydoo$captured_shaggy)
n <- ncol(CSH)
for(i in 1:n){
  CSH[,i] <- str_replace(CSH[,i],"TRUE", "1")
  CSH[,i] <- str_replace(CSH[,i],"FALSE", "0")
  CSH[,i] <- str_replace(CSH[,i],"NULL", "0")
}
CSH <- as.numeric(CSH[,i])
CSH <- data.frame(CSH)
for (i in unique(CSH)){
  print(cumsum(CSH[i]) )
}

for (i in unique(CSH)){
  CSH[i] = print(cumsum(CSH[i]))
}
CSH

capu <- cbind.data.frame(cumsum(CF),cumsum(CD),cumsum(CV),cumsum(CSO),cumsum(CSH))
  
## graph

plot(x=NA, y=NA, xlim=c(1,603), ylim=c(0,100), xlab = "épisode", ylab = "capture")
legend(x=10, y=100, names, coli, col, title = "Noms")
title(main = "Qui a été attrapé au fil des épisodes?")
labels
n = ncol(capu)

for (i in 1:n){
  lines(x = 1:603 , y = capu[,i], col=coli[i])
}

coli <-c("blue","purple","red","green","brown")

#######################################

# qui a attrapé combien de monstres 

X <- c(sum(scoobydoo$caught_fred==TRUE),
       sum(scoobydoo$caught_daphnie==TRUE),
       sum(scoobydoo$caught_velma==TRUE),
       sum(scoobydoo$caught_shaggy==TRUE),
       sum(scoobydoo$caught_scooby==TRUE))  

names <-c("Fred","Daphnie","Velma","Shaggy","Scooby")
caught <- cbind.data.frame(X, names)

table(caught)

c <- caught %>% 
  ggplot(aes(names, X, fill=names))+
  geom_col()+
  labs(title = "Qui a attrapé combien de monstres", x = "", y="")
c

# qui a été capturé combien de fois

Y <- c(sum(scoobydoo$captured_fred==TRUE),
       sum(scoobydoo$captured_daphnie==TRUE),
       sum(scoobydoo$captured_velma==TRUE),
       sum(scoobydoo$captured_shaggy==TRUE),
       sum(scoobydoo$captured_scooby==TRUE))

captured <-cbind.data.frame(Y,names)

b <- captured %>% 
  ggplot(aes(names, Y, fill=names))+
  geom_col()+
  labs(title = "Qui a été capturé combien de fois", x = "", y="")

b

# qui a démasqué combien de fois

Z <- c(sum(scoobydoo$unmask_fred==TRUE),
       sum(scoobydoo$unmask_daphnie==TRUE),
       sum(scoobydoo$unmask_velma==TRUE),
       sum(scoobydoo$unmask_shaggy==TRUE),
       sum(scoobydoo$unmask_scooby==TRUE))

unmask <-cbind.data.frame(Z,names)

a <- unmask %>% 
  ggplot(aes(names, Z, fill=names))+
  geom_col()+
  labs(title = "Qui a demasqué combien de fois", x = "", y="")


scoobydoo %>% 
  ggplot()+
  aes(x = names, fill = names) +
  geom_bar()



a
b
c

##########################

#motivation des monstres

table(scoobydoo$motive)




scoobydoo %>% 
  ggplot() +
  aes(year, as.numeric(scoobydoo$imdb), fill = monster_real) +
  geom_col()





# durée en fonction du type
ggplot(scoobydoo, aes(run_time, format)) + geom_point(aes(color = format))

# note en fct de la durée
scoobydoo %>% 
  ggplot()+
  aes(run_time, imdb2)+
  geom_point()+
  aes(color=format) +
  annotate("segment", x = 83, xend = 97, y = 5.1, yend = 6.1, color = "green", size = 1) +
  annotate("segment", x = 70, xend = 80, y = 4.3, yend = 4.3, color = "brown", size = 1) +
  annotate("text", x = 57, y = 4.4, label = "note minimale", color = "brown") +
  annotate("segment", x = 38, xend = 48, y = 9.4, yend = 9.4, color = "red", size = 1) +
  annotate("text", x = 62, y = 9.5, label = "note maximale", color = "red")




# platforme en fct des saisons
ggplot(scoobydoo, aes(season, network)) + geom_point()

scoobydoo %>% 
  ggplot()+
  aes(x = season, y = network, color = network)+
  geom_point()

# Moyenne des notes en fonction du format
moyf <- scoobydoo %>% 
  group_by(format) %>% 
  summarise(moy = mean(imdb2,na.rm=T))

moyf

# Moyenne des notes en fonction des décénnies
scoobydoo %>% 
  group_by(decennie) %>% 
  summarise(moy = mean(imdb2,na.rm=T))


# Note moyenne
notemoy <- scoobydoo %>% 
  summarise(moy= mean(imdb2,na.rm=T))

notemoyC <- scoobydoo %>% 
  filter(format=="Crossover") %>% 
  summarise(moy= mean(imdb2,na.rm=T))
notemoyM <- scoobydoo %>% 
  filter(format=="Movie") %>% 
  summarise(moy= mean(imdb2,na.rm=T))
notemoyMT <- scoobydoo %>% 
  filter(format=="Movie (Theatrical)") %>% 
  summarise(moy= mean(imdb2,na.rm=T))
notemoyTV <- scoobydoo %>% 
  filter(format=="TV Series") %>% 
  summarise(moy= mean(imdb2,na.rm=T))
notemoyTVs <- scoobydoo %>% 
  filter(format=="TV Series (segmented)") %>% 
  summarise(moy= mean(imdb2,na.rm=T))

# Moyenne des vues en fonction du format
scoobydoo %>% 
  group_by(format) %>% 
  summarise(moy = mean(as.numeric(engagement),na.rm=T))


# Moyenne des notes en fonction des saisons
scoobydoo %>% 
  group_by(season) %>% 
  summarise(moy = mean(imdb2, na.rm=T))



scoobydoo %>% 
  group_by(season) %>% 
  ggplot() +
  aes(season, imdb2, fill = season)+
  geom_boxplot() +
  geom_hline(yintercept = as.numeric(notemoy), color="red")




reg <- lm(as.numeric(imdb) ~ season, data = scoobydoo)
summary(reg)
plot(reg)

reg2 <- lm(as.numeric(engagement) ~ season + imdb2, data = scoobydoo)
summary(reg2)
plot(reg2)

reg3 <- lm(non_suspect ~ arrested, data = scoobydoo)


summary(scoobydoo$non_suspect)



sum(as.numeric(scoobydoo$monster_amount))
sum(as.numeric(scoobydoo$monster_real==TRUE))


#osef
scoobydoo %>% 
  ggplot() +
  aes(x = culprit_amount, y = suspects_amount, color = monster_real) +
  geom_point()

#osef
scoobydoo %>% 
  ggplot() +
  aes(culprit_amount, monster_amount, color=culprit_gender) +
  geom_point()


## Note en fct que le monstre soit réel ou non

scoobydoo %>% 
  ggplot() +
  aes(year, as.numeric(scoobydoo$imdb), fill = monster_real) +
  geom_col()
## problème avec imdb2!!!

## moyenne des notes en fonction des années
scoobydoo %>% 
  group_by(year) %>% 
  summarise(moy = mean(imdb2, na.rm=T)) %>% 
  ggplot() +
  aes(year, moy, fill = year) +
  scale_color_binned()+
  geom_col() +
  geom_hline(yintercept = as.numeric(notemoy)) +
  geom_hline(yintercept = as.numeric(notemoyC), color = "red") +
  geom_hline(yintercept = as.numeric(notemoyM), color = "blue") +
  geom_hline(yintercept = as.numeric(notemoyMT), color = "dark blue") +
  geom_hline(yintercept = as.numeric(notemoyTV), color = "green") +
  geom_hline(yintercept = as.numeric(notemoyTVs), color = "dark green") +
  labs(title="moyennes des notes en fonction des années", subtitle = "avec les références des moyennes en fonction du format")



#moyenne des notes en fonction des années
moy_note <- scoobydoo %>% 
  group_by(year) %>% 
  summarise(moy = mean(imdb2, na.rm=T))


## Note moyenne en fct de fred à attrapé le monstre

cf <- scoobydoo %>% 
  filter(captured_fred==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

cv <- scoobydoo %>% 
  filter(captured_velma==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

cd <- scoobydoo %>% 
  filter(captured_daphnie==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

csc <- scoobydoo %>% 
  filter(captured_scooby==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

csh <- scoobydoo %>% 
  filter(captured_shaggy==TRUE) %>% 
  summarise(moy = mean(imdb2, na.rm=T))

CAP <- cbind(cf, cd, cv, csh, csc)

colnames(CAP) <- names

CAP


# nb de fois ou qlqun ? attrap? un monstre en fct des d?c?nnies

# cr?er un facteur qui dit qui ? attrap?:

captuF <- scoobydoo %>% 
  filter(captured_fred==TRUE)

captuD <- scoobydoo %>% 
  filter(captured_daphnie==TRUE)

captuV <- scoobydoo %>% 
  filter(captured_velma==TRUE)

captuSC <- scoobydoo %>% 
  filter(captured_scooby==TRUE)

captuSH <- scoobydoo %>% 
  filter(captured_shaggy==TRUE)









## Genre en fonction des années et du fait qu'il soit réels


ggplot(scoobydoo) +
  geom_point(aes(x = year, y = monster_amount, color = monster_real)) + 
  facet_wrap(~ genre)



ggplot(scoobydoo) +
  geom_point(aes(x = decennie, y = monster_amount, color = monster_real)) +
  facet_wrap(~genre)
  

ggplot(scoobydoo) +
  geom_col(aes(x = decennie, y = monster_amount, fill = genre))



##

table(scoobydoo$motive)
cor.test(scoobydoo$motive, scoobydoo$imdb2)

library(FactoMineR)

motive <- cbind.data.frame(scoobydoo$motive)
motive

motives <- tab.disjonctif(motive)
colnames(motives) <- c("Abduction","Anger","Assistance","Automated","Bully","Competition",
                       "Conquer","Counterfeit","Entertainment","Experimentation","Extortion",
                       "Food","Haunt","Imagination","Inheritance","Loneliness","Mistake",
                       "Natural Resource","NULL","Preservation","Production","Safety",
                       "Simulation","Smuggling","Theft","Training","Treasure","Trespassing")
motives

cor.test(motives, scoobydoo$imdb2)
cor(motives)

motive2 <- cbind.data.frame(motives, as.numeric(scoobydoo$imdb), year(scoobydoo$date_aired))

ggplot(scooby) +
  geom_point(aes(x = date_aired, y = imdb, color = motive2)) + 
  facet_wrap(~ monsters_gen)

scoobydoo %>% 
  ggplot()+
  geom_point()+
  aes(year, imdb2, color=season)+
  facet_wrap(~motive2)


##nb d'épisodes par année
plot(table(scoobydoo$year))

scoobydoo %>% 
  ggplot()+
  geom_bar()+
  aes(year, fill=season)



####

install.packages("fastDummies")
library(fastDummies)
corrr<-scoobydoo %>% 
  select(imdb2, motive2) %>% 
  dummy_cols(select_columns = "motive2") %>% 
  select(!motive2) %>% 
  filter(!is.na(imdb2)) %>% 
  cor()
install.packages("broom")
library(broom)
cor2 <- as.data.frame(tidy(corrr[-1,1]))
colnames(cor2)=c("motif","corrélation avec la note")
cor2[,1]<-c("Abduction","Competition","Conquer","Natural Resource","Other","Preservation",
                              "Smuggling","Theft","Treasure")
View(cor2)
