library(tidyverse)
library(Distance)
library(mrds)

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
birds_det <- ds(birds, truncation = 400,  formula = ~ Treatment)
birds_hr <- ds(birds, truncation = 400, key = "hr")
birds_hr_tre <- ds(birds, truncation = 400, key = "hr",  formula = ~ Treatment)
birds_hr_loc <- ds(birds, truncation = 400, key = "hr",  formula = ~ Location)
birds_hr_sit <- ds(birds, truncation = 400, key = "hr",  formula = ~ Site)

plot(birds_hr_loc)
summary(birds_hr_loc)
gof_ds(birds_hr_loc)
summarize_ds_models(birds_hr_loc, birds_det, birds_hr, birds_hr_tre, birds_hr_sit)

#summary to get abundance and density
summary(birds_hr_loc)
######### not in here??
