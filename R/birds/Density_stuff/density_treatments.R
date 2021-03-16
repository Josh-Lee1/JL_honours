library(tidyverse)
library(Distance)
library(dplyr)

#introduce data
birds <- read.csv("Data/Raw/Birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
birds <- birds %>%
  group_by(Site) %>%
  mutate(species_richness = n_distinct(Species)) %>% 
  rename(distance = Distance) %>% 
  rename(Region.Label = Site) %>%
  filter(!is.na(distance))%>% 
  as.data.frame()
birds$Area <- '628'
birds$Effort <- '1'
birds$Sample.Label <- '1'
birds$Area <- as.numeric(birds$Area)
birds$Effort <- as.numeric(birds$Effort)

#split
burntbirds <- filter(birds, Fire == "Burnt")
unburntbirds <- filter(birds, Fire == "Unburnt")

#run dist
burnt_hr_loc <- ds(burntbirds, truncation = 400, key = "hr",  formula = ~ Location)

unburnt_hr_loc <- ds(unburntbirds, truncation = 400, key = "hr",  formula = ~ Location)

#stratify for species
species_level_dht2_burnt <- dht2(burnt_hr_loc, flatfile = burntbirds, stratification = "object", strat_formula = ~Species)

species_level_dht2_unburnt <- dht2(unburnt_hr_loc, flatfile = unburntbirds, stratification = "object", strat_formula = ~Species)

#observe
plot(burnt_hr_loc)
summary(burnt_hr_loc)
gof_ds(burnt_hr_loc)

plot(unburnt_hr_loc)
summary(unburnt_hr_loc)
gof_ds(unburnt_hr_loc)


#getting species lists for habitats
birds2 <- birds %>%  
  mutate(habitat = ifelse(Formation == "Rainforest", 1, 2)) %>% 
  select(Species, habitat) %>% 
  distinct() %>% 
  group_by(Species) %>% 
  summarise_all(sum)

#consolidate dfs
species_level_dht2_burnt <- select(species_level_dht2_burnt, Species, Abundance, Abundance_se, LCI, UCI)
species_level_dht2_burnt$Fire <- c("Burnt")
species_level_dht2_unburnt <- select(species_level_dht2_unburnt, Species, Abundance, Abundance_se, LCI, UCI)
species_level_dht2_unburnt$Fire <- c("Unburnt")


all_densities <- species_level_dht2_burnt %>% 
  rbind(species_level_dht2_unburnt) %>% 
  filter(Species != "Total") %>% 
  left_join(birds2, by = "Species")
all_densities$Species = with(all_densities, reorder(Species, Abundance))


all_densities$habitat <- as.character(all_densities$habitat)

#all plot
ggplot(data = all_densities,
       aes(Species,
           Abundance,
           ymax = Abundance+Abundance_se,
           ymin = Abundance-Abundance_se,
           colour = habitat)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip()


#split by habitat
rfonly<- filter(all_densities, habitat == "1")
dsonly<- filter(all_densities, habitat == "2")
both<- filter(all_densities, habitat == "3")

#plot them
rf_only_plot <- ggplot(data = rfonly,
       aes(Species,
           Abundance,
           ymax = UCI,
           ymin = LCI,
           colour = Fire)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Rainforest Only Birds")
ggsave(rf_only_plot, filename = "Outputs/densities/rf_only.png")

ds_only_plot <- ggplot(data = dsonly,
       aes(Species,
           Abundance,
           ymax = UCI,
           ymin = LCI,
           colour = Fire)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Dry Sclerophyll Only Birds")
ggsave(ds_only_plot, filename = "Outputs/densities/ds_only.png")

both_plot <- ggplot(data = both,
       aes(Species,
           Abundance,
           ymax = UCI,
           ymin = LCI,
           colour = Fire)) + 
  geom_point() +
  geom_pointrange() +
  coord_flip() +
  ggtitle("Birds in Both Habitats")
ggsave(both_plot, filename = "Outputs/densities/bothhabs.png")

