ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')

race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

utr <- merge(x = ultra_rankings, y = race, by = 'race_year_id')

utr[ ,c('time', 'event', 'city', 'start_time', 'race_year_id')] <- list(NULL)


