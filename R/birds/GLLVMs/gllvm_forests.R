library(gllvm)
library(tidyverse)
library(lattice)
library(janitor)
library(ggpubr)
library(ggplotify)

#read in data created in Traits.R
df <- read.csv("Data/Processed/ALLdata.csv") %>% 
  select(-c(X)) %>% 
  mutate(Burnt = Fire =="Burnt") %>% 
  mutate(Rainforest = Formation =="Rainforest")

#make some string changes so it will work with gllvm
df$Burnt<- as.integer(as.logical(df$Burnt))
df$Rainforest<- as.integer(as.logical(df$Rainforest))

df$location.id <- as.numeric(factor(df$Location, levels=unique(df$Location)))

#split into 2 dfs
rf <- df %>% 
  filter(Formation == "Rainforest") %>% 
  select(-c(id)) %>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0) %>% 
  ungroup() %>% 
  as.data.frame()
rf$id <- as.integer(as.factor(rf$Site))
  

  
ds <- df %>% 
  filter(Formation == "Dry Sclerophyll") %>% 
  select(-c(id)) %>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0) %>% 
  ungroup() %>% 
  as.data.frame()
ds$id <- as.integer(as.factor(ds$Site))


#format each df for 4thCM, then run in gllvm
##br###################################################################
Xrf <-  rf %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>%
  select(-c(id)) %>% 
  as.matrix()


yrf <-  rf %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id) %>% 
  as.matrix()


TRrf <- rf %>% 
  dplyr::select (Species,
                 X99_Body_mass_average_8,
                 X163_Food_Fruit_10,
                 X164_Food_Nectar_or_pollen_10,
                 X165_Food_Seeds_10,
                 X166_Food_Foliage_or_herbs_10,
                 X168_Food_Terrestrial_invertebrates_10,
                 X169_Food_Terrestrial_vertebrates_10) %>% 
  dplyr::distinct() %>%
  dplyr::select(-Species) %>% 
  rename(Frugivore = X163_Food_Fruit_10,
         Nectarivore = X164_Food_Nectar_or_pollen_10,
         Granivore = X165_Food_Seeds_10,
         Folivore = X166_Food_Foliage_or_herbs_10,
         Insectivore = X168_Food_Terrestrial_invertebrates_10,
         Carnivore = X169_Food_Terrestrial_vertebrates_10,
         Size = X99_Body_mass_average_8) %>% 
  as.matrix()


#running Burnt Rainforest 
fit_4thrf <- gllvm(yrf, Xrf, TRrf, family = "negative.binomial", num.lv = 2, 
                   formula =  ~ (Litter.Depth + Litter.Cover + Understory + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Canopy.Cover)


rf_coef<- coefplot(fit_4thrf, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthrf <- fit_4thrf$fourth.corner
a <- max( abs(fourthrf) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thrf <- levelplot((as.matrix(fourthrf)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4thrf



##bs###################################################################
Xds <-  ds %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>%
  select(-c(id)) %>% 
  as.matrix()


yds <-  ds %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id) %>% 
  as.matrix()


TRds <- ds %>% 
  dplyr::select (Species,
                 X99_Body_mass_average_8,
                 X163_Food_Fruit_10,
                 X164_Food_Nectar_or_pollen_10,
                 X165_Food_Seeds_10,
                 X166_Food_Foliage_or_herbs_10,
                 X168_Food_Terrestrial_invertebrates_10,
                 X169_Food_Terrestrial_vertebrates_10) %>% 
  dplyr::distinct() %>%
  dplyr::select(-Species) %>% 
  rename(Frugivore = X163_Food_Fruit_10,
         Nectarivore = X164_Food_Nectar_or_pollen_10,
         Granivore = X165_Food_Seeds_10,
         Folivore = X166_Food_Foliage_or_herbs_10,
         Insectivore = X168_Food_Terrestrial_invertebrates_10,
         Carnivore = X169_Food_Terrestrial_vertebrates_10,
         Size = X99_Body_mass_average_8) %>% 
  as.matrix()


#running Burnt Sclerophyll 
fit_4thds <- gllvm(yds, Xds, TRds, family = "negative.binomial", num.lv = 2, 
                   formula =  ~ (Litter.Depth + Litter.Cover + Understory + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Canopy.Cover)


ds_coef<- coefplot(fit_4thds, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthds <- fit_4thds$fourth.corner
b <- max( abs(fourthds) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thds <- levelplot((as.matrix(fourthds)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-b, b, length = 100), scales = list(x = list(rot = 45)))
plot.4thds
