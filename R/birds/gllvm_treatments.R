library(gllvm)
library(tidyverse)
library(lattice)
library(janitor)

#read in data created in Traits.R
df <- read.csv("Data/Processed/ALLdata.csv") %>% 
  select(-c(X)) %>% 
  mutate(Burnt = Fire =="Burnt") %>% 
  mutate(Rainforest = Formation =="Rainforest")

#make some string changes so it will work with gllvm
df$Burnt<- as.integer(as.logical(df$Burnt))
df$Rainforest<- as.integer(as.logical(df$Rainforest))

df$location.id <- as.numeric(factor(df$Location, levels=unique(df$Location)))

#split into 4 dfs
br <- df %>% 
  filter(Treatment == "RainforestBurnt") %>% 
  select(-c(id)) %>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0) %>% 
  ungroup()
br$id <- as.integer(as.factor(br$Site))

ur <- df %>% 
  filter(Treatment == "RainforestUnburnt") %>% 
  select(-c(id))%>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0)%>% 
  ungroup()
ur$id <- as.integer(as.factor(ur$Site))

bs <- df %>% 
  filter(Treatment == "Dry SclerophyllBurnt") %>% 
  select(-c(id))%>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0)%>% 
  ungroup()
bs$id <- as.integer(as.factor(bs$Site))

us <- df %>% 
  filter(Treatment == "Dry SclerophyllUnburnt") %>% 
  select(-c(id))%>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0)%>% 
  ungroup()
us$id <- as.integer(as.factor(us$Site))

#format each df for 4thCM
##br###################################################################
Xbr <-  br %>% 
  dplyr::select (Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>% 
  as.matrix()

ybr <-  br %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id) %>% 
  as.matrix()


TRbr <- br %>% 
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
fit_4thbr <- gllvm(ybr, Xbr, TRbr, family = "negative.binomial", num.lv = 2, 
                   formula = ybr ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


coefplot(fit_4thbr, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthbr <- fit_4thbr$fourth.corner
a <- max( abs(fourthbr) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thbr <- levelplot((as.matrix(fourthbr)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4thbr



##bs###################################################################
Xbs <-  bs %>% 
  dplyr::select (Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>% 
  as.matrix()

ybs <-  bs %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id) %>% 
  as.matrix()

TRbs <- bs %>% 
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
         Size = X99_Body_mass_average_8)




#running Burnt Sclerophyll 
fit_4thbs <- gllvm(ybs, Xbs, TRbs, family = "negative.binomial", num.lv = 2, 
                  formula = ybs ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                    (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                    (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                  row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                  randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


coefplot(fit_4thbs, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthbs <- fit_4thbs$fourth.corner
a <- max( abs(fourthbs) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thbs <- levelplot((as.matrix(fourthbs)), xlab = "Environmental Variables", 
                       ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                       at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4thbs
