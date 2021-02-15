library(tidyverse)

birds<- read.csv("Data/Raw/birds.csv") %>%
  mutate_all(list(~na_if(.,""))) %>% 
  filter(!is.na(Position))

n_distinct(birds$Species)
unique(birds$Species)

nofly<-filter(birds, Position != "Flyover") 
n_distinct(nofly$Species)
unique(nofly$Species)