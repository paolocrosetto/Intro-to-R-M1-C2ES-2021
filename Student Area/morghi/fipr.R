install.packages("magick")
## lire les données 
SW <- swim_years
#garder les colones importantes 
SW = SW %>%
  select(-type,-country,-grp_id)
#suppression des NA 
SW <- SW %>% 
  drop_na()

# counter les medailles pour chaque pays 
medal_count<- SW %>% filter(!is.na(medal))%>%
  group_by(abb, medal) %>%
  summarize(Count=length(medal)) 

# ordonner  les pays par nombre de medailles 
ord_med <- medal_count %>%
  group_by(abb) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(abb)
medal_count$abb <- factor(medal_count$abb, levels=ord_med$abb)

# plot
ggplot(medal_count, aes(x=abb, y=Count, fill=medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("les medailles de chaque pays dans l'histoire de la compéttition ") +
  theme(plot.title = element_text(hjust = 0.5))

#nombre de medailles par sex
ggplot(SW,aes(x= gender ,fill= medal))+
  geom_bar()+
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("nombre de medailles par sex ") +
  theme(plot.title = element_text(hjust = 0.5))

#Facet 
ggplot(SW,aes(x= gender ,fill= medal))+
  facet_wrap(~ year)+
  geom_bar()+
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("nombre de medailles par sex pour chaque année ") +
  theme(plot.title = element_text(hjust = 0.5))  

#distrubtion des atheletes dans le monde 

noc <- read_csv("noc_regions.csv",
                col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))
noc = noc %>%
  rename(abb = NOC)
# Add regions to data and remove missing points
data_regions <- SW %>% 
  left_join(noc,by="abb") %>%
  filter(!is.na(region))

# sous ensemble pour les jeux 1980 -> 2016 et counter les athletes de chaque pays 
rio <- data_regions %>% 
  filter(year == "2016") %>%
  group_by(region) %>%
  summarize(Rio = length(unique(athlete)))

Arnhem_et_Veenendaal<- data_regions %>% 
  filter(year == "1980") %>%
  group_by(region) %>%
  summarize(Arnhem = length(unique(athlete)))

# Create data for mapping
world <- map_data("world")
mapdat <- tibble(region=unique(world$region))
mapdat <- mapdat %>% 
  left_join(Arnhem_et_Veenendaal, by="region") %>%
  left_join(rio, by="region")
mapdat$Arnhem[is.na(mapdat$Arnhem)] <- 0
mapdat$Rio[is.na(mapdat$Rio)] <- 0
world <- left_join(world, mapdat, by="region")


# Plot: Arnhem et Veenendaal  1980
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Arnhem)) +
  labs(title = "Arnhem et Veenendaal  1980",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "navy"),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient2(low="white",high = "red")


# Plot:  Rio 2016
ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Rio)) +
  labs(title = "Rio 2016",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "navy"),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient2(low="white",high = "red")


##les pays qui n'ont pas de médaille d'or mais ils ont les autres
data_abb_medal <- dcast(medal_count, abb ~ medal)
data_abb_medal[is.na(data_abb_medal)] <- 0
no_gold_data <- subset(data_abb_medal, Gold == 0 & Silver>0 & Bronze>0)
print("les pays qui n'ont pas de médaille d'or mais ils ont les autres")
no_gold_data$abb


##countries where women never won gold medal but men has

all_medal_sex <- SW%>% group_by(abb, medal, gender) %>%
  summarise(total = n())
all_medal_sex.wide <- dcast(all_medal_sex, abb ~ medal+gender)
all_medal_sex.wide[is.na(all_medal_sex.wide)] <- 0
no_women_gold <- subset(all_medal_sex.wide, Gold_Women ==0 & Gold_Men>0 )
no_men_gold <- subset(all_medal_sex.wide, Gold_Women>0 & Gold_Men==0 )
print("countries where women never won gold medal but men has")
no_women_gold$abb
print("countries where men never won gold medal but women has")
no_men_gold$abb


# medal_continent
continent <-read_csv("data.csv")
continent = continent %>%
  rename(abb = Three_Letter_Country_Code)

medal_continent <- SW %>%
left_join(continent,by="abb") %>%
  filter(!is.na(Continent_Name))

medal_continent<- medal_continent %>% filter(!is.na(medal))%>%
  group_by(year,Continent_Name) %>%
  summarize(Count=length(medal)) 

WP3 <- ggplot(data = medal_continent, aes(x = year, y = Count, group=Continent_Name, color=Continent_Name)) +
  geom_line() +
  geom_point() +
  ggtitle("Nombre de médailles entre 1980 et 2016") +
  ylab("Nombre de médailles") +
  xlab("Année")+
  theme_classic()+
  view_follow(fixed_x = TRUE, 
              fixed_y = FALSE) +
  transition_reveal(year)


WP3 <- animate(WP3, end_pause = 15)

WP3















WP <- ggplot(data = medal_continent) +
  geom_col(mapping = aes(x = Continent_Name, y = Count), 
           fill = "darkcyan") +
  theme_classic() +
  xlab("Région") +
  ylab("Nombre de téléphones (en milliers)") +
  transition_states(year,
                    transition_length = 2,
                    state_length = 1, 
                    wrap = TRUE) +
  ggtitle("Année : {closest_state}")

WP
