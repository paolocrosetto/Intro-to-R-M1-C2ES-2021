library(tidyverse)

library(babynames)

skimr::skim(bn)

bn <- babynames

View(bn)

names(bn)

table(bn$sex)

# chercher la moyenne des années des bébés

bn %>% 
  mutate(age = 2021 - year) %>%

mutate(bn, age = 2021 - year )
    

age <- mutate(bn, age = 2021 - year )

df %>%
  filter(year == 2008) %>% 
  group_by(sex) %>% 
  top_n(1)

library(babynames)
library(tidyverse)



