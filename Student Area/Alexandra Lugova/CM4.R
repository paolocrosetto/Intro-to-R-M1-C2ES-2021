# CM 4: join & reshape

library(tidyverse)

# Joining datasets

## using the `nycflights13` dataset again
library(nycflights13)
planes <- nycflights13::planes
planes

airports <- nycflights13::airports
airports

flights <- nycflights13::flights
flights

## joining different datasets: example

# problem: do newer planes fly the longest routes from NYC?
# check if tailnum est unique for chanque planes - unique key?
planes %>% group_by(tailnum) %>% filter(n()>1) # 0 rows => it's unique
#or
planes %>%  group_by(tailnum) # number of observations = number of groups => it's unique

#`full_join()` keeps everything, adds `NA`
#`inner_join()` keeps only matched data
#`left_join()` keeps *all* keys in the *left* df
## the default `left_join()`
distance <- flights %>% select(tailnum, distance)
yearbuilt <- planes %>% select(tailnum, year)
answer <- left_join(distance, yearbuilt, by = "tailnum")
answer
#or - distance %>% left_join(yearbuilt, by = "tailnum")

## answering our question: the answer - there seems to be no connection between the age of the plane and the distance of flights
answer %>% group_by(year) %>% 
  summarise(dist = mean(distance, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = dist))+geom_point()+
  geom_smooth(method = "lm")
# or regression - modern planes fly shorter routes (negative dependence)
answer %>% 
  lm(distance~year, data = .) %>% 
  broom :: tidy()

## joining exercise - how many flights through NYC land in an airport whose altitude is > 1000mt? 
# note: 1 mètre = 3,28084 feet
alt_df <- airports %>% select(faa,alt) %>% mutate(alt = alt/3.28024) %>% rename(dest = faa)
answer <- left_join(flights, alt_df, by = "dest") %>% filter(alt>1000) 
answer # answer is 10291 flights

## joining three datasets - how old are the planes that fly to airports whose altitude is >1000mt?
answer <- left_join(flights,yearbuilt, by = "tailnum")
answer <- left_join(answer, alt_df, by = "dest")
answer %>% filter(alt>1000) %>% summarise(avgyear = mean(year.y, na.rm = TRUE))
answer %>% filter(alt<=1000) %>% summarise(avgyear = mean(year.y, na.rm = TRUE))

# messy data -> tidy data

## a simple dataset in four versions
table1 #the best
table2
table3
table4a  #cases
table4b  #population

## tidy data: the `tidyr` package
## reshaping to longer `pivot_longer`
table4a
table4a %>% pivot_longer(!country) # ! can be -

table4a %>% pivot_longer(!country, names_to = "year", values_to = "val")

## reshaping to wider
table2
table2 %>% pivot_wider(names_from = type, values_from = count)

## separating: from one to more variables
table3
separate(table3, col = rate, into = c("cases", "population"))

## sep - to indicate the separation symbol
separate(table3, col = rate, into = c("cases", "population"), sep = "/")

## convert - to convert variables from character to int or dbl
separate(table3, col = rate, into = c("cases", "population"), convert = TRUE)

## uniting: from several to one variable
table5
unite(table5, year, century, year)

## to separate without _
unite(table5, year, century, year, sep = "")



