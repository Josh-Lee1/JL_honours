library(gllvm)
library(tidyverse)
library(lattice)

#read in data created in Traits.R
df <- read.csv("Data/Processed/ALLdata.csv") %>% 
  select(-c(X)) %>% 
  mutate(Burnt = Fire =="Burnt") %>% 
  mutate(Rainforest = Formation =="Rainforest")

#make some string changes so it will work with gllvm
df$Burnt<- as.integer(as.logical(df$Burnt))
df$Rainforest<- as.integer(as.logical(df$Rainforest))

df$location.id <- as.numeric(factor(df$Location, levels=unique(df$Location)))

#run the mod
mod<- gllvm(formula = Count ~ Burnt * Rainforest + location.id, data = df, num.lv = 2, 
      family = "negative.binomial")

#check assumptions
plot(mod)
mod$params
coefplot(mod)

mod.null <-  gllvm ( data = df , formula = Count ~ Burnt + Rainforest + location.id, num.lv = 2, family = "negative.binomial")

anova(mod.null , mod)

####incorportaing traits
X <-  df %>% 
  dplyr::select (Burnt, location.id, Rainforest, id, Litter.Depth, Litter.Cover, Understory, Mid.height, Canopy.Cover) %>% 
  dplyr::distinct()

X$location.id <- as.character(X$location.id)

X <-  model.matrix(object = ~ Burnt * Rainforest+location.id,  data = X)

y <-  df %>% 
  dplyr::select (Species, id, Count) %>% 
  tidyr::pivot_wider(values_from = Count, names_from = Species, id_cols = id)%>%
  dplyr::select (-id)

#looks like I have to do one guild at a time...?
TR <- df %>% 
  dplyr::select (Species,
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
         Carnivore = X169_Food_Terrestrial_vertebrates_10)

#running trait mods

model.formula <-  as.formula(paste0( "y ~ Burnt * Rainforest + Burnt:Rainforest:Frugivore + ", paste0 ("location.id", 2:15, collapse = "+")))

model.gllvm.traits <-  gllvm (y = y, X = X, TR = TR,  formula = model.formula, num.lv = 2, family = "negative.binomial")

plot(model.gllvm.traits)
coefplot(model.gllvm.traits)

##4th Corner Model
fit_4th <- gllvm(y, X, TR, family = "negative.binomial", num.lv = 2, 
                 formula = y ~ (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) +
                   (Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover) : 
                   (Frugivore + Nectarivore + Granivore + Folivore + Insectivore + Carnivore), seed = 123,
                 row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                 randomX = ~ Litter.Depth + Litter.Cover + Understory + Mid.height + Canopy.Cover)


coefplot(fit_4th, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourth <- fit_4th$fourth.corner
a <- max( abs(fourth) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4th <- levelplot((as.matrix(fourth)), xlab = "Environmental Variables", 
                      ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                      at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4th
