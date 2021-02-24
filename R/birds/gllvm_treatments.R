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

#split into 4 dfs
br <- df %>% 
  filter(Treatment == "RainforestBurnt") %>% 
  select(-c(id)) %>% 
  group_by(Species) %>% 
  mutate(total = sum(Count)) %>% 
  filter(total > 0) %>% 
  ungroup() %>% 
  as.data.frame()
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

#format each df for 4thCM, then run in gllvm
##br###################################################################
Xbr <-  br %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>%
  select(-c(id))
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


#running Burnt Rainforest 
fit_4thbr <- gllvm(ybr, Xbr, TRbr, family = "negative.binomial", num.lv = 2, 
                   formula =  ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


br_coef<- coefplot(fit_4thbr, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthbr <- fit_4thbr$fourth.corner
a <- max( abs(fourthbr) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thbr <- levelplot((as.matrix(fourthbr)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4thbr



##bs###################################################################
Xbs <-  bs %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>%
  select(-c(id)) %>% 
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
         Size = X99_Body_mass_average_8) %>% 
  as.matrix()


#running Burnt Sclerophyll 
fit_4thbs <- gllvm(ybs, Xbs, TRbs, family = "negative.binomial", num.lv = 2, 
                   formula =  ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


bs_coef<- coefplot(fit_4thbs, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthbs <- fit_4thbs$fourth.corner
b <- max( abs(fourthbs) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thbs <- levelplot((as.matrix(fourthbs)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-b, b, length = 100), scales = list(x = list(rot = 45)))
plot.4thbs

##ur###################################################################
Xur <-  ur %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>%
  select(-c(id)) %>% 
  as.matrix()


yur <-  ur %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id) %>% 
  as.matrix()


TRur <- ur %>% 
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


#running Unburnt Rainforest
fit_4thur <- gllvm(yur, Xur, TRur, family = "negative.binomial", num.lv = 2, 
                   formula =  ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


ur_coef<- coefplot(fit_4thur, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthur <- fit_4thur$fourth.corner
c <- max( abs(fourthur) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thur <- levelplot((as.matrix(fourthur)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-c, c, length = 100), scales = list(x = list(rot = 45)))
plot.4thur

##us###################################################################
Xus <-  us %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct() %>%
  select(-c(id)) %>% 
  as.matrix()


yus <-  us %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id) %>% 
  as.matrix()


TRus <- us %>% 
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


#running Unburnt Rainforest
fit_4thus <- gllvm(yus, Xus, TRus, family = "negative.binomial", num.lv = 2, 
                   formula =  ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                     (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                     (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore + Size), seed = 123,
                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                   randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


us_coef<- coefplot.gllvm(fit_4thus, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourthus <- fit_4thus$fourth.corner
d <- max( abs(fourthus) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4thus <- levelplot((as.matrix(fourthus)), xlab = "Environmental Variables", 
                        ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                        at = seq(-d, d, length = 100), scales = list(x = list(rot = 45)))
plot.4thus

#making combo figure
#####having issues with coefficient plots
br_coef
ur_coef
bs_coef
us_coef
plot.4thbr
plot.4thur
plot.4thbs
plot.4thus

ggarrange(plot.4thbr, plot.4thur, plot.4thbs, plot.4thus, 
          labels = c("A", "B", "C", "D"),
          ncol = 1, nrow = 4)

ggarrange(br_coef, ur_coef, bs_coef, us_coef, 
          labels = c("A", "B", "C", "D"),
          ncol = 1, nrow = 4)
