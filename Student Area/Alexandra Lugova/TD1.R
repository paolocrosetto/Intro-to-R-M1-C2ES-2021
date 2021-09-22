## TD 1

# attach package to workspace
library(nycflights13)

# import data to workspace
df <- flights

# view data
View(df)

# basic statistics for every variable
summary(df)

# exploration of data with skimr
install.packages("skimr")

skimr :: skim(df)

# attach tidyverse
library(tidyverse)

# extract rows from data
filter(df, month == 6)
filter(df, month == 6 & day ==1)

# variable names
names(df)

# tabulate data
table(df$origin)

# how many flights de JFK 7 juillet 2013?
filter(df, month == 7 & day == 7 & year == 2013 & origin == "JFK" )

# flights in march-april
filter(df, month == 3 | month == 4)

# flights in march-august
filter(df, month %in% c(3,4,5,6,7,8))
filter(df, month %in% c(3:8))

# arrange data - ex. order by hour
arrange(df, sched_dep_time)
arrange(df, -sched_dep_time)
arrange(df, origin, month, day)

# select columns
select(df,origin, dest)
select(df, -month, -day)

# variable starting/ending with
select(df, starts_with("arr"))
select(df, ends_with("time"))

# variable contains
select(df, contains("t"))


# find all flights from JFK 4 may 2013, select destination
filter(df, origin == "JFK" & day == 4 & month == 5 & year == 2013)
select(df, dest)

# PIPE - %>% - et apres 
df %>% 
  filter(origin == "JFK" & day == 4 & month == 5 & year == 2013) %>%
  select(dest)

# list flights from 12 to 14 on 3 july, show hour of departure and arrival, sort by airport
df %>%
  filter(day == 3 & month == 7 & dep_time %in% c(1200:1400)) %>%
  arrange(origin) %>%
  select(dep_time, arr_time) 

# rename variables
rename(df, mois = month)

# change data inplace: df <- df ...

# create new variable by manipulation
# normalize year to years from now
df %>%
  mutate(year_from_now = 2021 - year) %>%
  select(year, year_from_now)

# mutate with logic
df %>%
  mutate(isJFK = origin == "JFK") %>%
  select(origin, isJFK)

# summarize data
# mean delay of flights 
df %>%
  summarize(mean_delay_depart = mean(dep_delay, na.rm = TRUE))
# na.rm - for elimination of nul objects

# group data by column
# group by origin
group_by(df, origin)
group_by(df, origin, month)            

# mean delay by aeroport
summarize(group_by(df, origin), mean_delay = mean(dep_delay, na.rm = TRUE))

# mean speed of flights departing 11-13 by month
df %>%
  filter(dep_time %in% c(1100:1300)) %>%
  mutate(speed = air_time / distance) %>%
  group_by(month) %>%
  summarize(meanspeed = mean(speed, na.rm = T)) %>%
  select(month, meanspeed)

# mean delay of flights by carrier by each month
df %>% 
  group_by(carrier, month) %>% 
  summarize(meandelay = mean(dep_delay, na.rm = T)) %>% 
  select(carrier, month, meandelay)

# max departure delay for each airport by day
df %>% 
  group_by(origin, day) %>% 
  summarize(maxdelay = max(dep_delay, na.rm = T)) %>% 
  select(origin, day, maxdelay)

# mean air time by airport
df %>%
  group_by(origin) %>% 
  summarize(meanair = mean(air_time, na.rm = T)) %>% 
  select(origin, meanair)
# the longest flights are from JFK

# select top/tail rows
df %>% 
  top_n(5)
df %>% 
  tail(5)
