library(tidyverse)
library(Distance)
library(mrds)
library(lme4)
library(sjPlot)
library(sjmisc)

#format data like species_density.R
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


#isolate Lewin's Honeyeater and add zeros
sites <- birds %>% 
  select(Region.Label, Location, Formation, Fire, Treatment, Area, Effort, Sample.Label) %>% 
  distinct() %>% 
  mutate(Species = "Lewin's Honeyeater")
  
leho <- birds %>%
  select(Region.Label, Location, Formation, Fire, Treatment, Species, Count, distance, Area, Effort, Sample.Label)%>% 
  filter(Species == "Lewin's Honeyeater") %>% 
  bind_rows(sites) %>% 
  unique() %>% 
  replace(., is.na(.), "0")

#distance bits
leho_hr_loc <- ds(leho, truncation = 400, key = "hr", monotonicity = "strict", formula = ~ Location)


leho_hr_loc_dht2 <- dht2(leho_hr_loc, flatfile = leho, stratification = "object", strat_formula = ~Treatment)
