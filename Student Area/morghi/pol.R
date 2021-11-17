
SW = SW %>%
  select(-type)

# count number of medals awarded to each Team
medal_counts_art <- SW %>% filter(!is.na(medal))%>%
  group_by(abb, medal) %>%
  summarize(Count=length(medal)) 

# order Team by total medal count
levs_art <- medal_counts_art %>%
  group_by(abb) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(abb)
medal_counts_art$abb <- factor(medal_counts_art$abb, levels=levs_art$abb)

# plot
ggplot(medal_counts_art, aes(x=abb, y=Count, fill=medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Historical medal counts from Art Competitions") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(SW,aes(x= gender ,fill= medal))+
  geom_bar()+
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
    ggtitle("nombre de medailles par sex ") +
    theme(plot.title = element_text(hjust = 0.5))

ggplot(SW,aes(x= gender ,fill= medal))+
  facet_wrap(~ year)+
  geom_bar()+
    scale_fill_manual(values=c("gold1","gray70","gold4")) +
    ggtitle("nombre de medailles par sex de chaqu'année ") +
    theme(plot.title = element_text(hjust = 0.5))  



noc <- read_csv("noc_regions.csv",
                col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))
noc = noc %>%
  rename(abb = app)
# Add regions to data and remove missing points
data_regions <- SW %>% 
  left_join(noc,by="abb") %>%
  filter(!is.na(region))

# Subset to Games of interest and count athletes from each country
rio <- data_regions %>% 
  filter(year == "2016") %>%
  group_by(region) %>%
  summarize(Rio = length(unique(athlete)))

# Create data for mapping
world <- map_data("world")
mapdat <- tibble(region=unique(world$region))
mapdat <- mapdat %>% 
  left_join(rio, by="region")
mapdat$Rio[is.na(mapdat$Rio)] <- 0
world <- left_join(world, mapdat, by="region")


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




