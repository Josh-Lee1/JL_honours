library(tidyverse)
library(Distance)
library(mrds)
library(lme4)
library(sjPlot)
library(sjmisc)

birds <- read.csv("Data/Raw/Birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
birds <- birds %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species)) %>% 
  rename(distance = Distance) %>% 
  rename(Region.Label = Site) %>%
  as.data.frame()
birds$Area <- '628'
birds$Effort <- '1'
birds$Sample.Label <- '1'
birds$Area <- as.numeric(birds$Area)
birds$Effort <- as.numeric(birds$Effort)

#Fitting detection function
birds_hr_loc <- ds(birds, truncation = 400, key = "hr",  formula = ~ Location)

#getting species lists for habitats
birds2 <- birds %>%  
  mutate(habitat = ifelse(Formation == "Rainforest", 1, 2)) %>% 
  select(Species, habitat) %>% 
  distinct() %>% 
  group_by(Species) %>% 
  summarise_all(sum) %>% 
  mutate(rfonly = ifelse(habitat == 1, 1, 0),
         dsonly = ifelse(habitat == 2, 1, 0),
         bothhab = ifelse(habitat == 3, 1, 0))

#Stratafying results by species
species_level_dht2 <- dht2(birds_hr_loc, flatfile = birds, stratification = "object", strat_formula = ~Species)

species_densities <- species_level_dht2 %>% 
  left_join(birds2, by = "Species") %>% 
  select(Species, Abundance, Abundance_se, habitat:bothhab) %>% 
  filter(Species != "Total") %>% 
  arrange(Abundance) %>% 
  mutate(Species=factor(Species, levels=Species))
species_densities$habitat <- as.character(species_densities$habitat)

#make a first trial plot
ggplot(data = species_densities,
       aes(Species,
           Abundance,
           ymax = Abundance+Abundance_se,
           ymin = Abundance-Abundance_se,
           colour = habitat)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip()

#try splitting by habitat
rfonly<- filter(species_densities, habitat == "1")
dsonly<- filter(species_densities, habitat == "2")
both<- filter(species_densities, habitat == "3")

ggplot(data = rfonly,
       aes(Species,
           Abundance,
           ymax = Abundance+Abundance_se,
           ymin = Abundance-Abundance_se)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip()
ggplot(data = dsonly,
       aes(Species,
           Abundance,
           ymax = Abundance+Abundance_se,
           ymin = Abundance-Abundance_se)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip()
ggplot(data = both,
       aes(Species,
           Abundance,
           ymax = Abundance+Abundance_se,
           ymin = Abundance-Abundance_se)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip()




#Get species lists for each habitat
##Rainforest, Both, Dry Scl
#leftjoin x3 to make dfs
#Plot abundances
#
#
#