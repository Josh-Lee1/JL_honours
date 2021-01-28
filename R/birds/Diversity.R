library(ggplot2)
library(tidyverse)

#Species Richness

birds <- read.csv("Data/Raw/Birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
birds <- birds %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species))

sr <- ggplot(birds, aes(x = Treatment, y = species_richness)) + geom_boxplot() +ylab("# species")+ ggtitle("All")
ggplot(birds, aes(x = Location, y = species_richness, fill = Treatment)) + geom_boxplot() +ylab("# species")

birds100 <- filter(birds, Distance<=100)
birds100 <- birds100 %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species))
sr100 <- ggplot(birds100, aes(x = Treatment, y = species_richness)) + geom_boxplot() +ylab("# species") + ggtitle("<=100")
write.csv(birds100, file = "Data/Processed/birds100.csv")

birds80 <- filter(birds, Distance<=80)
birds80 <- birds80 %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species))
sr80 <- ggplot(birds80, aes(x = Treatment, y = species_richness)) + geom_boxplot() +ylab("# species")+ ggtitle("<=80")


birds50 <- filter(birds, Distance<=50)
birds50 <- birds50 %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species))
sr50<- ggplot(birds50, aes(x = Treatment, y = species_richness)) + geom_boxplot() +ylab("# species")+ ggtitle("<=50")

print(sr)
print(sr100)
print(sr80)
print(sr50)

meandistances<- aggregate(birds[12], list(birds$Site, birds$Treatment), mean, na.rm = TRUE)
distplot<- ggplot(birds, aes(x = Treatment, y = Distance)) + geom_boxplot() +ylab("Distance")+ ggtitle("Bird Distances")
print(distplot)
