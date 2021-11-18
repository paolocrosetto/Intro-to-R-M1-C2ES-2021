Quels sont les caractéristiques les plus rares dans ce jeu de donnée ?
  ```{r}

count_feature <- hike_data_clean %>%  count(fct_rev(fct_infreq(features))) 
colnames(count_feature) <- c("carac","compte")
count_feature[1:5,] %>% 
  ggplot(aes(x =carac,y= compte )) + 
  geom_col(fill = "seagreen4",colour = "darkgreen") + labs(title = "Quels sont les caractéristiques les plus rares ?") +
  ylab("Nombre de randonnées") +
  xlab("Caractéristiques") +  
  theme(plot.title = element_text(face = "bold",size = 14)) + 
  theme_minimal()
```