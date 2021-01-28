library(tidyverse)
library(Distance)
library(mrds)

birds <- read.csv("Data/Raw/Birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
birds <- birds %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species)) %>% 
  rename(distance = Distance) %>% 
  as.data.frame()


#simple Abundance plot
birds100 <- birds100 %>%
  group_by(Site) %>%
  summarise("total_birds" = sum(Count, na.rm = TRUE)) %>% 
  left_join(birds100, by = "Site") %>% 
  as.data.frame()

totbirds100<- ggplot(birds100, aes(x = Treatment, y = birds100$total_birds)) + geom_boxplot() +ylab("# birds")
print(totbirds100)

#play with Distance package
## detection function
birds_det <- ds(birds, truncation = 400, key = "hr",  formula = ~ Formation)
plot(birds_det)
