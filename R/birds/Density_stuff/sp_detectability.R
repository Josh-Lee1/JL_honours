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


#
sites <- birds %>% 
  select(Region.Label, Location, Formation, Fire, Treatment, Area, Effort, Sample.Label) %>% 
  distinct()
  
#Do the Analysis:
##just have to replace species name in first line

birds %>% filter(Species=="Black-faced Monarch")->oo
woom<-ds(data = oo,truncation = 400,key="hn",monotonicity = "strict")
oo %>%
  right_join(sites,c("Region.Label", "Location", "Formation", "Fire",  "Treatment","Sample.Label","Effort","Area"))->pp
out<-dht2(woom,flatfile=pp,strat_formula=~Treatment)

