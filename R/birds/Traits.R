library(tidyverse)

birds<- read.csv("Data/Raw/birds.csv")
traits <- read.csv("Data/Raw/trait_data.csv") %>% 
  rename(Species = X3_Taxon_common_name_2)

birdtraits <- traits %>% 
  right_join(birds100, traits, by = "Species") %>% 
  filter(Species %in% birds100$Species)

#Trying to get the data to work with gllvm

UG <- read.csv("Data/Processed/UGpts.csv") %>% 
  select(Group.1, sum) %>% 
  rename(Site = Group.1, UGpts = sum)

veg <- read.csv("Data/Raw/veg.csv") %>% 
  filter(Point == 10 | Point == 20 | Point == 30 | Point == 40 | Point == 50) %>% 
  select(Site,
         Litter.Cover, 
         Litter.Depth, 
         Growth..2m, 
         Canopy.Cover) %>% 
  left_join(UG, by = "Site")%>% 
  tidyr::replace_na(list(Growth..2m = 40)) %>%
  group_by(Site) %>% 
  summarize(Litter.Depth = mean(Litter.Depth),
            Litter.Cover = mean(Litter.Cover),
            Understory = mean(UGpts),
            Mid.height = mean(Growth..2m),
            Canopy.Cover = mean(Canopy.Cover))

birdspread<- read.csv("Data/Processed/birdspread.csv") %>% 
  select(-c(X, Site))

traits<- birdtraits %>% 
  select(Species,
         X99_Body_mass_average_8,
         X163_Food_Fruit_10:X170_Food_Carrion_10) %>% 
  distinct()
write.csv(traits, "Data/Processed/traitspre.csv")
traits<- read.csv("Data/Processed/traitspost.csv") %>% 
  rename(sp = Species)
traits1 <- rownames_to_column(traits, var = "Species") %>% as_tibble()

traits1$Species <- as.character(traits1$Species)


df0 <- reshape(data.frame(cbind(birdspread, veg)), direction = "long", varying =
                colnames(birdspread), v.names = "Count", timevar = "Species") 
  
df0$Species <- as.character(df0$Species)

bspread <- birdspread %>% 
  t()
id <- rownames(bspread)
bspread <- cbind(id=id, bspread)
rownames(bspread)<-c(1:99)
Species <- rownames(bspread)
bspread <- cbind(Species=Species, bspread)

df1 <- bspread %>% 
  as.data.frame() %>% 
  select(Species:id) %>% 
  left_join(df0, by = "Species")%>% 
  left_join(traits1, by = "Species")%>%
  select(-c(sp))%>% 
  rename(sp.id = Species, Species = id.x, id = id.y)

##fix veg info
veginfo <- birdtraits %>% 
  select(Site,
         Fire, 
         Formation, 
         Treatment) %>% 
  distinct()

#put density and diversity in here
density<- read.csv("Data/Processed/density.csv") %>%
  select(-c(X)) %>% 
  distinct() %>% 
  select(Site, Location, Estimate_abundance, Estimate_density)
diversity <- read.csv("Data/Processed/diversity.csv") %>% 
  select(Site, Species_richness, Shannon_diversity, Simpson_diversity)


df2 <- df1 %>% 
  right_join(veginfo, by = "Site") %>% 
  select(sp.id, Species, Site, Treatment, everything()) %>% 
  left_join(density, by = "Site")%>% 
  left_join(diversity, by = "Site") %>% 
  unique()
  
write.csv(df2, "Data/Processed/ALLdata.csv")

