library(tidyverse)
library(vegan)
library(labdsv)
library(iNEXT)
library(lme4)
library(sjPlot)
library(sjmisc)


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
  spread(Species, Count, fill = 0) %>% 
  as.data.frame()

write.csv("Data/Processed/birdspread.csv")

birdspread1 <- birdspread[,-1]
rownames(birdspread1) <- birdspread[,1]
divdata <-  t(birdspread1)


#calculating diversity indices
diversity <- iNEXT(divdata, q=0, datatype = "abundance")
ggiNEXT(diversity, type = 1)

summary(diversity)
diversesum<- diversity$AsyEst

#Simple method with vegan package
veganshan<- birdspread1 %>% 
  vegan::diversity(index = "shannon") %>% 
  as.data.frame() %>% 
  dplyr::rename(Shannon = ".") %>% 
  rownames_to_column(var = "Site")

vegansimp<- birdspread1 %>%
  vegan::diversity(index = "simpson") %>% 
  as.data.frame() %>% 
  dplyr::rename(Simpson = ".") %>% 
  rownames_to_column(var = "Site")

vegandiv <- dplyr::left_join(vegansimp, veganshan, by = c("Site" = "Site"))

#get the ouputs from iNEXT
vegnames <- vegandiv %>% 
  rownames_to_column(var = "r.name")

sumq0 <- diversesum %>% 
  filter(Diversity == "Shannon diversity") %>% 
  rownames_to_column(var = "r.name") %>% 
  right_join(vegnames, by = "r.name")
  
#messing around to get a nice df
treatinfo<-birds %>% 
  select("Site", "Treatment", "Fire", "Formation", "Location") %>% 
  distinct()

diversity <- sumq0 %>% 
  right_join(diversesum, by = c("Site.x" = "Site")) %>% 
  select("Site.y", "Diversity.y", "Observed.y", "Estimator.y", "s.e..y",  "LCL.y", "UCL.y") %>% 
  rename(Site="Site.y", Diversity="Diversity.y", Observed="Observed.y", Estimator="Estimator.y", s.e.="s.e..y",  LCL="LCL.y", UCL="UCL.y") %>% 
  left_join(treatinfo, by = "Site") %>% 
  dplyr::relocate(Treatment, .after = Site)
  
div4mod <- diversity %>% 
  select(Site, Treatment, Fire, Formation, Location, Diversity, Observed) %>% 
  spread(Diversity, Observed) %>% 
  rename(Shannon_diversity = "Shannon diversity") %>%
  rename(Simpson_diversity = "Simpson diversity") %>% 
  rename(Species_richness = "Species richness")
write.csv(div4mod, "Data/Processed/diversity.csv")

#make model
## Shannon
shandiv<- lmer(Shannon_diversity ~ Formation * Fire +(1|Location), data = div4mod)
shandiv2<- lmer(Shannon_diversity ~ Formation + (1|Location), data = div4mod)

summary(shandiv)
plot(shandiv)
anova(shandiv2,shandiv)

plot_model(shandiv, type = "int")

## Simpson
simpdiv<- lmer(Simpson_diversity ~ Formation * Fire +(1|Location), data = div4mod)
simpdiv2<- lmer(Simpson_diversity ~ Formation + (1|Location), data = div4mod)

summary(simpdiv)
plot(simpdiv)
anova(simpdiv2,simpdiv)
confint(simpdiv)


plot_model(simpdiv, type = "int")

ggplot(div4mod, aes(Treatment, Shannon_diversity))+geom_boxplot()

