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

siteinfo <- birds %>% 
  select(Region.Label, Fire, Formation, Treatment, Location) %>% 
  distinct() %>% 
  rename(Site = Region.Label)

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
#birds_det <- ds(birds, truncation = 400,  formula = ~ Treatment)
#birds_hr <- ds(birds, truncation = 400, key = "hr")
#birds_hr_tre <- ds(birds, truncation = 400, key = "hr",  formula = ~ Treatment)
birds_hr_loc <- ds(birds, truncation = 400, key = "hr",  formula = ~ Location)
#summarize_ds_models(birds_hr_loc, birds_det, birds_hr, birds_hr_tre)

plot(birds_hr_loc)
summary(birds_hr_loc)
gof_ds(birds_hr_loc)


#summary to get abundance and density
summary(birds_hr_loc)

abund_table <- summary(birds_hr_loc)$dht$individuals$N
abund_table$lcl <- abund_table$ucl <- abund_table$df <- NULL
colnames(abund_table) <- c("Site", "Estimate_abundance", "se_abundance",
                           "CV_abundance")

dense_table <- summary(birds_hr_loc)$dht$individuals$D
dense_table$lcl <- dense_table$ucl <- dense_table$df <- NULL
colnames(dense_table) <- c("Site", "Estimate_density", "se_density",
                           "CV_density")
density <- abund_table %>%
  left_join(dense_table, by = "Site") %>% 
  left_join(siteinfo, by = "Site") %>% 
  filter(Site != "Total")
write.csv(density, "Data/Processed/density.csv")

#Plot and ANOVA
abundplot<- ggplot(density, aes(x = Treatment, y = Estimate_abundance)) + geom_boxplot() +ylab("Abundance")
print(abundplot)

density.lmer<- lmer(Estimate_density ~ Formation * Fire +(1|Location), data = density)
density.lmer2<- lmer(Estimate_density ~ Formation + (1|Location), data = density)

summary(density.lmer)
plot(density.lmer)
anova(density.lmer2,density.lmer)

plot_model(density.lmer, type = "int")
