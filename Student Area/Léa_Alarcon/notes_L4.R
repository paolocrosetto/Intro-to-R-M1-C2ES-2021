library(tidyverse)

library(nycflights13)

df <- flights

df

planes <- planes
airports <- airports

## left join
## ajoute à une base de régérence des données qui sont sur une autre base

# est-ce que le ID est unique sur UNE des deux bases?
planes %>% count(tailnum) %>% filter(n != 1)

# check! ça marche parce que la base où on cherche des avions présents >1 fois est VIDE

flights_planes <- df %>% 
  select(-year) %>% 
  left_join(planes, by = "tailnum")

# join bases avec clé avec nom différent

# join airports et flights. l'acronyme de l'aéroport s'appelle 'dest' dans df et 'faa' dans airports

# est-ce que la clé est unique? 
df %>%  count(dest)
airports %>% count(faa) %>% filter(n != 1)

# façon 1: utiliser un vecteur nommé
df %>% 
  left_join(airports, by = c("dest" = "faa"))

# façon 2: renommer variable dans données origine
df %>% 
  rename("faa" = "dest") %>% 
  left_join(airports, by = "faa")

# façon 3: renommer la variable dans les données de 'droite' (additionnelles)
airports %>% 
  rename("dest" = "faa") %>% 
  right_join(df, by = "dest")


## autres fonctions join()

# inner_join()

#intersection des deux bases

# joiner les vols avec les aéroports
airports %>% 
  filter(alt > 1000) %>% 
  inner_join(df, by = c("faa" = "dest"))

# !!! perte d'info !!!

# full_join()

#union de deux bases

df %>% 
  full_join(airports, by = c("dest" = "faa"))


# exercice 1.
# *do newer planes fly the longest routes from NYC?*

# vols dans flights
# âge des avions dans planes

flights %>% 
  select(-year) %>% 
  left_join(planes, by = "tailnum") %>% 
  ggplot(aes(x = year, y = distance))+
  geom_point()+
  geom_smooth()

## pas de relation apparaente -- faudrait faire des régressions (prochaine fois)

# exercice 2.
# combien de vols atterissent à plus de 1000mt?

flights %>% 
  left_join(airports, by = c("dest" = "faa")) %>% 
  mutate(alt_mt = alt/3.28084) %>% 
  filter(alt_mt > 1000) %>% 
  group_by(dest, name, alt_mt) %>% 
  summarise(n = n())

# exercice 3
#
flights %>% 
  left_join(airports, by = c("dest" = "faa")) %>% 
  mutate(alt_mt = alt/3.28084) %>% 
  filter(alt_mt > 1000) -> vols_plus_1000

vols_plus_1000 %>% 
  select(-year) %>% 
  left_join(planes, by = 'tailnum') %>% 
  summarise(n = n()) %>% 
  .$n

# diffrentes fçons de donner une réponse

# 1. avec moyenne et sd
vols_plus_1000 %>% 
  summarise(meanyear = mean(year, na.rm = T),
            sdyear = sd(year, na.rm = T))

# 2. avec un graphique
vols_plus_1000 %>% 
  ggplot(aes(x = year)) +
  geom_density()

vols_plus_1000 %>% 
  ggplot(aes(x = year)) +
  geom_histogram()

## on a appris à faire de join!!!


## partie 2 -- PIVOT

## pivot_longer --> prend une base de données "large" et la rend "longue"

# table4a a deux problèemes
# - la variable "year" est cachée dans le titre d'aures variables
# - les valuers des cas n'ont pas de nom

t4a_tidy <-table4a %>% 
  pivot_longer(cols = !country, names_to = "année", values_to = "cas")


t4b_tidy <- table4b %>% 
  pivot_longer(cols = !country, names_to = "année", values_to = "pop")



## joining to get table1 back again
t4a_tidy %>% left_join(t4b_tidy)


## pivot_longer exercice 2

## données de la banque mondiale
wbp <- world_bank_pop

wbp %>% 
  pivot_longer(cols = !country & !indicator, 
               names_to = "year", 
               values_to = "value") -> wbp_long

## pivot_wider -> transforme une base de données en "largeur". 

wbp_long %>% 
  pivot_wider(names_from = year,
              values_from = value)


## exercice 2: avec table2
# transformer table2 en table1

table2 %>% 
  pivot_wider(names_from = type, values_from = count)


## montrer qu'il s'agit d'opérations inverses

wbp %>% 
  pivot_longer(cols = !country & !indicator, 
               names_to = "year", values_to = "val") %>% 
  pivot_wider(names_from = year, values_from = val)


## separate

## séparer une case quand il y a plus qu'une valeur à son intérieur

table3 %>% 
  separate(col = rate, 
           into = c("cases", "population", "nimportequoi"), 
           sep = c(3,8))

## separate exercice 2

wbp %>% 
  separate(col = indicator, 
           into = c("sert_a_rien", "territory", "indicator")) %>% 
  select(-sert_a_rien)


## l'inverse de separate c'est unite

# exercice: transformer table5 en table1

table5 %>% 
  unite(col = year, century, year, sep = "") %>% 
  separate(rate, into = c("cases", "population"), sep = "/") %>% 
  mutate(cases = as.double(cases),
         population = as.integer(population),
         yearf = as.factor(year))


## difference betweencharacter vectors and factors
chr <- c("a", "a", "b", "c")
fct <- as.factor(chr)

