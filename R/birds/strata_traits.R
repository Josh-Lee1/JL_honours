library(tidyverse)

traits1 <- read.csv("Data/Processed/traitspost.csv")
traits2 <- read.csv("Data/Raw/trait_data.csv") %>% 
  select(X3_Taxon_common_name_2,
         X9_Family_common_name_2,
         X10_Family_scientific_name_2,
         X41_Global_IUCN_status_2015_5,
         X60_NSW_status_2015_6) %>% 
  rename(Species = "X3_Taxon_common_name_2")
birds <- read.csv("Data/Raw/birds.csv") %>% 
  filter(Distance <= 100) %>% 
  select(Species, Position) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  mutate_all(list(~na_if(.,"")))


sdspread <- birds %>% 
  pivot_wider(names_from = Position, values_from = value) %>% 
  select(-c("Midstorey, Understorey", "Ground/Understorey", "NA")) %>% 
  left_join(traits2, by = "Species")

sdspread$Understorey[29]=1
sdspread$Ground[29]=1

write.csv(sdspread, "Data/Processed/strata_data.csv")

#git test