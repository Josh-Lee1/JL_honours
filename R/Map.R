library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)

#prepare df
birds <- read.csv("Data/Raw/birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
siteinfo <- dplyr::select(birds, "Site", "Longitude", "Latitude", "Elevation", "Location", "Formation", "Fire", "Treatment")

#prepare map
Aus<- st_read("Data/Raw/1259030001_ste11aaust_shape/STE11aAust.shp")
sites <- st_as_sf(siteinfo, coords = c("Longitude", "Latitude"), crs = st_crs(4326))

#make map
map <- ggplot() +
  geom_sf(data=Aus) +
  geom_sf(data=sites, aes(colour = sites$Treatment))+
  xlim(140, 154)+
  ylim(37, 28)
print(map)

map1 <- ggplot() +
  geom_sf(data=Aus) +
  geom_sf(data=sites, aes(colour = sites$Treatment))+
  xlim(152.2, 153.5)+
  ylim(29.5, 28.3)
print(map1)
