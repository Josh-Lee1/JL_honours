library(tidyverse)
library(vegan)
library(labdsv)


#Species Richness

birds <- read.csv("Data/Raw/Birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
birds <- birds %>%
  filter(Notes != "Out of Survey") %>% 
  dplyr::group_by(Site) %>%
  dplyr::mutate(species_richness = n_distinct(Species))


#plotting sr
ggplot(birds, aes(x = Treatment, y = species_richness)) + geom_boxplot() +ylab("# species")+ ggtitle("All")
ggplot(birds, aes(x = Location, y = species_richness, fill = Treatment)) + geom_boxplot() +ylab("# species")

#investigating cutoff
meandistances<- aggregate(birds[12], list(birds$Site, birds$Treatment), mean, na.rm = TRUE)
distplot<- ggplot(birds, aes(x = Treatment, y = Distance)) + geom_boxplot() +ylab("Distance")+ ggtitle("Bird Distances")
print(distplot)
##### 100m looks like it gets rid of the outliers

#crop to 100m
birds100 <- filter(birds, Distance<=100)
birds100 <- birds100 %>%
  group_by(Site) %>%
  dplyr::mutate(species_richness = n_distinct(Species))
sr100 <- ggplot(birds100, aes(x = Treatment, y = species_richness)) + geom_boxplot() +ylab("# species") + ggtitle("<=100")
write.csv(birds100, file = "Data/Processed/birds100.csv")

#reorganising df
birdspread<- birds100 %>% 
  select(Site, Species, Count) %>% 
  group_by(Site, Species) %>% 
  summarise_all(funs(sum)) %>% 
  spread(Species, Count, fill = 0)

#calculating diversity index
vegan::diversity(birdspread, index = "shannon")

