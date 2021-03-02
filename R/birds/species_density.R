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

#Fitting detection function
birds_hr_loc <- ds(birds, truncation = 400, key = "hr",  formula = ~ Location)

#Stratafying results by species
species_level_dht2 <- dht2(birds_hr_loc, flatfile = birds, stratification = "object", strat_formula = ~Species)

species_densities <- species_level_dht2 %>% 
  left_join(birds2, by = "Species") %>% 
  select(Species, Abundance, Abundance_se, habitat:bothhab)






#Get species lists for each habitat
##Rainforest, Both, Dry Scl
#leftjoin x3 to make dfs
#Plot abundances
#
#
#