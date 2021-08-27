###########################################################################
## merci de votre participation et bon courage pour le reste du Master!  ##
###########################################################################

# je charge les librairies dont j'ai besoin
library(eurostat) ## <- pour télécharger des données eurostat 
library(tidyverse) ## <- notre cher ami le tidyverse
library(stringr) ## <- pour manipuler des chaines de charactères


############ 1 donnees econ simples

## une tache simple: how did the unemploment rate of the US vary in time? (plot)
## quels est le mois avec plus de unemplyment?
## y a t il une relation entre la durée du chomage et le taux de chomage? (plot)
## y a t il une relation entre la population totale et le nombre de chomeurs? (plot)

df <- economics_long
?economics_long

df %>% spread(variable, value)

##this logn format is really bad. let's spread it
df <- df %>% select(-value01) %>% spread(variable, value)


## let's have a look at the data
df <- df %>% mutate(ur = unemploy/pop) 

#plot
df %>% ggplot(aes(x = date, y = ur))+geom_line()

#mois avec le plus de chomage
df %>% arrange(-ur) %>% filter(row_number()<=4)

#quel est l'année ou on a plus de chomeurs, en moyenne?
df %>% separate(date, into=c("annee","mois","jour")) %>% 
       group_by(annee) %>% summarise(avg_ur = mean(ur)*100) %>%  arrange(-avg_ur)

#quel est le mois ou on a plus de chomeurs, en moyenne?
df %>% separate(date, into=c("annee","mois","jour")) %>% 
  group_by(mois) %>% summarise(avg_ur = mean(ur)*100) %>%  arrange(-avg_ur)


#correlation
df %>% ggplot(aes(ur*100,uempmed))+geom_point()+geom_smooth(method = "lm")

#time series with size variable
df %>% ggplot(aes(x = date, y = ur*100))+geom_point(aes(size = uempmed))

#pop/chomeurs
df %>% ggplot(aes(pop, unemploy))+geom_point()+geom_smooth(method = "lm")


#######################
# number of births in Europe
## plot the number of births in 2015 for all countries (Note: just the countries, nt their subdivisions!)
## plot the number of births evolution from 1990 to 2016 for all those countries for which we have the full data
library(eurostat)

# je cherhce les données eurostat
sources <- search_eurostat("birth")

# je télécharge les données de naissance
df <- get_eurostat("demo_r_births", time_format = "num")

# comment se concentrer seulement sur les pays? problème: pays est un FACTOR
df %>% filter(nchar(geo)==2) ## <- ceci ne marchera pas

## ceci va marcher
df2015 <- df %>% mutate(geo = as.character(geo)) %>%
  filter(nchar(geo)==2 & time == 2015) 

df2015 %>% ggplot(aes(reorder(geo,-values), values))+geom_col()

## plot final avec les réponses à la première questions: dans quels pays il y avait plus de naissances en 2015?
df2015 %>% ggplot(aes(reorder(geo,values), values))+
  geom_col(fill="chartreuse4")+theme_light()+
  geom_hline(yintercept = 100000, color = "red")+coord_flip()

##évolution par année et pays
dfEvolution <- df %>% mutate(geo = as.character(geo)) %>%
  filter(nchar(geo)==2) 

#et voilà le graph. n n'y comprend rien!
dfEvolution %>% ggplot(aes(time, values))+   geom_line()  ## <- ceci ne marchera pas!

dfEvolution %>% ggplot(aes(time, values, group=geo, color=geo))+  geom_line()

#on va affiner: seulement IT, FR, DE, UK, ES
dfEvolution %>% 
  filter(geo %in% c("IT","FR","DE","UK","ES")) %>% 
  ggplot(aes(time, values,  color=geo))+
  geom_line()


########################" 2 eurostat data on PIB, 5 pays

## quelle est l'évoluton du PIB à prix courants pour les 5 grands pays EU
## Allemagne, France, Italie, Royaume-Uni, Espagne
## quelle est l'évolution du PB (plot)
## quelle est l'evolution de la croissance du PIB (plot)

## tache 2: eurostat PIB plot

## je cherche les données sur mon ordi (NOTEZ: le parcours est différent chez vous)
df <- read_csv("/home/paolo/Dropbox/Grenoble/Enseignement/UGA 2018 - M1 - Intro R/nama_10_gdp_1_Data.csv")

# je sélectionne la variable d'intérêt
df <- df %>% filter(UNIT == "Current prices, million euro") %>% 
            select(GEO, TIME, Value)
# je filtre les pays d'intérêt
df <- df %>% filter(GEO == "France" | 
                    GEO == "Germany (until 1990 former territory of the FRG)" | 
                    GEO == "Italy" | 
                    GEO == "Spain" )

#autre filtrage tout à fait équivalent!
df %>% filter(GEO %in% c("France", 'Italy', "Spain", "Germany (until 1990 former territory of the FRG)"))



#problem!! Value is a string!!
df %>% mutate(Value = as.numeric(Value)) ## <- ceci ne marchera pas

#ceci marchera
df <- df %>% mutate(Value = str_replace_all(Value, ",", ""))  
df <- df %>% mutate(Value = as.numeric(Value))  


##plotting -> évolution du PIB
df %>% ggplot(aes(x = TIME, y = Value))+geom_line(aes( color = GEO))

#taux de croissance du PIB nominal
tdc <- df %>% arrange(GEO, TIME) %>% group_by(GEO) %>%  
  mutate(growth = 100*(Value - lag(Value))/lag(Value))

# plotting
tdc %>% ggplot(aes(x = TIME, y = growth, group = GEO, color = GEO))+
  geom_line()+
  geom_hline(yintercept = 0)


####################################
### 4:  quelles villes en FRANCE avaient le plus de lits d'hotel 
###     par habitant en 2014?

## tâche 1:
eurostat::search_eurostat("tourism")

# lits par habitants des villes francaises et espagnoles en 2014

# la base de données avec les CODES des communes
beds <- eurostat::get_eurostat("urb_ctour",time_format="num" )

# la meme base avec les LABELS (=noms) des communes
beds_label <- eurostat::get_eurostat("urb_ctour", time_format = "num", type = "label" )
# je n'ai besoin que des noms et de l'indicateur
beds_label <- beds_label %>% select(indicator = indic_ur, city = cities)

# je merge les noms et les labels --> vu que les deux bases ont le même nombre d'observations et le même ordre je peux utiliser cbind (column bind)
beds <- cbind(beds, beds_label)

# je sépare le code en apys + code pour pouvoir sélectionner un pays
beds <- beds %>% mutate(code = cities) %>% separate(cities, into = c("country","dropme"), sep = 2) %>% select(-dropme, -indic_ur)

# je cherche un pays et une année -- dans ce cas, Italie 2004. Essayez France 2014 ou autre
beds <- beds %>% filter(indicator == "Number of bed-places in tourist accommodation establishments" & country == "UK" & time == 2014)


# je cherche les données de la population des villes
eurostat::search_eurostat("cities")
pop <- eurostat::get_eurostat("urb_cpop1", time_format = "num")

# je fais la même manip que pour les lits: je sépare les pays du code des villes, je sélectionne un pays et une année
# l'indicateur que je veux est DE1001V -> pop résidente totale
pop <- pop %>% mutate(code = cities) %>% separate(cities, into = c("country","dropme"), sep = 2) %>%
               select(-dropme) %>% filter(time == 2014 & country == "UK" & indic_ur == "DE1001V") %>% 
               select(code, population = values)

# je merge beds et pop -- je veux qu'il soit un 'tibble' data frame (pas important ici)
answer <- left_join(beds, pop, by = "code")

# je calcule le nombre de lit d'hotel par habitant et je met les observations dans l'ordre voulu
answer <- answer %>% mutate(bedsperhead = 1000*values/population) %>% arrange(-bedsperhead)


