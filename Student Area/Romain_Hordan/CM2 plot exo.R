# Exercice CM 2, plot
library(tidyverse)
library(nycflights13)
df= flights
df
q1 = df  %>% 
  filter(origin=="JFK") %>% 
  group_by(dest) %>% 
summarise(N=n()) %>% 
  top_n(10)
q1 %>% 
ggplot() + 
  aes(x= reorder(dest, -N),N) + geom_point()

