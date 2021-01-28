library(dplyr)

birds<- read.csv("Data/Raw/birds.csv")
traits <- read.csv("Data/Raw/trait_data.csv") %>% 
  rename(Species = X3_Taxon_common_name_2)

birdtraits <- traits %>% 
  right_join(birds100, traits, by = "Species") %>% 
  filter(Species %in% birds100$Species)


#a<- c(unique(birds$Species))
#b <- c(unique(birdtraits$Species))
#write.csv(a, "Data/Processed/a.csv")
#write.csv(b, "Data/Processed/b.csv")