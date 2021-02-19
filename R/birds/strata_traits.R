library(tidyverse)

traits <- read.csv("Data/Processed/traitspost.csv") 
birds <- read.csv("Data/Raw/birds.csv") %>% 
  filter(Distance <= 100) %>% 
  select(Species, Position) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  mutate_all(list(~na_if(.,"")))


sdspread <- birds %>% 
  pivot_wider(names_from = Position, values_from = value) %>% 
  select(-c("Midstorey, Understorey", "Ground/Understorey", "NA"))

sdspread$Understorey[29]=1
sdspread$Ground[29]=1

write.csv(sdspread, "Data/Processed/strata_data.csv")
