## CM3 - plots

library(tidyverse)

df <- read_tsv("/Users/alexandra.lugova/Documents/GitHub/Intro-to-R-M1-C2ES-2021/Lecture 3 - basic ggplot/data/DatasaurusDozen.tsv", show_col_types = FALSE)

table(df$dataset)

# calcule moyenne et dev st de chaque dataset

df %>% 
  group_by((dataset)) %>% 
  summarise(mean_x = mean(x), mean_y = mean(y), sd_x = sd(x), sd_y = sd(y))

plot(df)

# faire des plots - GGPLOT

mpg <- mpg

# main function
ggplot(mpg)

# aes to create axes
p <- ggplot(mpg, aes(x = displ, y = hwy))
p

# operateur "+" - choisir point/ligne/...
p + geom_point()

p + geom_smooth()

p + geom_point() + geom_smooth()

# smoother lineaire
p + geom_point() + geom_smooth(method = "lm")

# add reference line
p + geom_point() + geom_hline(yintercept = 25, color = 'red')

# add colors by class
p + geom_point(aes(color=class))

# different size points
p + geom_point(aes(size=cyl))

# colors and size
p + geom_point(aes(size = cyl, color=class))

# different shape of points
p + geom_point(aes(shape=fl))

# shape + color + size
p + geom_point(aes(color=manufacturer, shape =fl, size = cyl))

## exercice babynames
library(babynames)

# evolution du nom Mary
babynames %>% 
  filter(name == "Mary", sex == "F") -> Maries

Maries %>% 
  ggplot() +
  aes(x = year, y = prop) + 
  geom_line()

# make facets (divide plot in several parts)
p + geom_point() + facet_grid(.~fl) #in a row
p + geom_point() + facet_wrap(.~fl) #matrice

p + geom_point() + facet_grid(trans~.) #rows
p + geom_point() + facet_grid(.~trans) #columns

## types des plots
# 1 variable, discrete
# bar chart
mpg %>% 
  ggplot()+
  aes(x = manufacturer, fill = manufacturer) +
  geom_bar()

# bar chart color by different criteria
mpg %>% 
  ggplot()+
  aes(x = manufacturer, fill = drv) +
  geom_bar()

# ordonner les bars
mpg %>% 
  group_by(manufacturer) %>% 
  mutate(N = n()) %>% 
  ggplot()+
  aes(x = reorder(manufacturer, -N), fill = manufacturer) +
  geom_bar()

## 1 var, continue
# histogram -- densite
mpg %>% 
  ggplot()+
  aes(x = hwy) +
  geom_histogram()

mpg %>% 
  ggplot()+
  aes(x = hwy) +
  geom_density()
  
# dotplot 
mpg %>% 
  ggplot()+
  aes(x = hwy) +
  geom_dotplot()

# boxplot
mpg %>% 
  ggplot()+
  aes(x = hwy) +
  geom_boxplot()

# violin plot ~ densite
mpg %>% 
  ggplot()+
  aes(x = class, y = hwy) +
  geom_violin()

mpg %>% 
  ggplot()+
  aes(x = 1, y = hwy) +
  geom_violin()

## 2 var, continues
mpg %>% 
  ggplot(aes(x = hwy, y = cty)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed')

mpg %>% 
  ggplot(aes(x = hwy, y = cty)) +
  geom_jitter(width = 5, height = 5)+
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed')

## 2 var, 1 continue, 1 discrete
# boxplot
p <- mpg %>% 
  ggplot()+
  aes(x = class, y = hwy, color = class, fill = class)
p + geom_boxplot()

# inverser les axes
p + coord_flip() + geom_boxplot()

# raincloud plot
p + geom_point()
p + geom_jitter(heigth = 0, width = .2)

## 2 var, 2 discretes
mpg %>% 
  ggplot()+
  aes(x = class, y = drv)+
  geom_count()

## ou c'est:
table(mpg$class, mpg$drv)

## 3 var continues
mpg %>% group_by(year, drv) %>% summarise(n = n()) %>% 
  ggplot(aes(x = drv, y = year, fill = n)) +  geom_tile()

## pour chaque drive et n de cylindre, moyenne conso?
mpg %>% 
  group_by(drv, cyl) %>% 
  summarise(mean_conso = mean(hwy)) %>% 
  ggplot() +
  aes(x = drv, y = cyl, fill = mean_conso) +
  geom_tile()
  