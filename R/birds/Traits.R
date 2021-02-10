library(dplyr)

birds<- read.csv("Data/Raw/birds.csv")
traits <- read.csv("Data/Raw/trait_data.csv") %>% 
  rename(Species = X3_Taxon_common_name_2)

birdtraits <- traits %>% 
  right_join(birds100, traits, by = "Species") %>% 
  filter(Species %in% birds100$Species)

#Trying to get the data to work with gllvm

density<- read.csv("Data/Processed/density.csv")
diversity <- read.csv("Data/Processed/diversity.csv")
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
  group_by(Site) %>% 
  summarize(Litter.Depth = mean(Litter.Depth),
            Litter.Cover = mean(Litter.Cover),
            Understory = mean(UGpts, na.rm = TRUE),
            Mid.height = mean(Growth..2m, na.rm = TRUE),
            Canopy.Cover = mean(Canopy.Cover, na.rm = TRUE))

birdspread<- read.csv("Data/Processed/birdspread.csv") %>% 
  select(-c(X, Site))

traits<- birdtraits %>% 
  select(Species,
         Fire, 
         Formation, 
         Treatment,
         X99_Body_mass_average_8,
         X163_Food_Fruit_10:X173_Food_fish_or_invertebrates_Inland_waters_10) %>% 
  distinct()
write.csv(traits, "Data/Processed/traitspre.csv")
traits<- read.csv("Data/Processed/traitspost.csv") %>% 
  rename(sp = Species) %>% 
  rename(Species = X.1) %>% 
  select(-c(X))
traits$Species <- as.character(traits$Species)


df <- reshape(data.frame(cbind(birdspread, veg)), direction = "long", varying =
                colnames(birdspread), v.names = "Count", timevar = "Species") 
  
df$Species <- as.character(df$Species)

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
  left_join(df, by = "Species") %>% 
  left_join(traits, by = "Species")%>%
  select(-c(id.y, Species, id.x)) %>% 
  rename(Species = sp) %>% 
  select(Species, everything())
  
write.csv(df1, "Data/Processed/ALLdata.csv")

