library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)

#prepare df
birds <- read.csv("Data/Raw/birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
siteinfo <- dplyr::select(birds, "Site", "Longitude", "Latitude", "Elevation", "Location", "Formation", "Fire", "Treatment")

#prepare map
Aus<- st_read("Data/Raw/1259030001_ste11aaust_shape/STE11aAust.shp")
sites <- st_as_sf(siteinfo, coords = c("Longitude", "Latitude"), crs = st_crs(4326))
wha <- st_read("Data/Raw/world_heritage_public/world_heritage_public.shp") %>% 
  filter(NAME == "Gondwana Rainforests of Australia")

#make map
map <- ggplot() +
  geom_sf(data=Aus) +
  geom_sf(data=sites, aes(colour = sites$Treatment))+
  xlim(140, 154)+
  ylim(37, 28)
print(map)

map1 <- ggplot() +
  geom_sf(data=Aus, fill = "#FFFFCC") +
  geom_sf(data=sites, aes(colour = sites$Treatment))+
  xlim(152.2, 153.7)+
  ylim(29.5, 28.2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  theme(legend.position = c(0.88, 0.1))+
  scale_fill_discrete(labels = c("Burnt Dry Sclerophyll", "Unburnt Dry Sclerophyll", "Burnt Rainforest", "Unburnt Rainforest"))+
  scale_color_manual(values = c("#FF6600", "#66CC00", "#FF00FF", "#0000FF"))

print(map1)

#add in 
#WHA
#GIS things?
#Inset
#
study_area <- st_as_sfc(st_bbox(sites))

map <- ggplot() +
  geom_sf(data=Aus, fill = "white") +
  geom_sf(data = study_area, fill = "red", color = "red", size = 1.2) +
  xlim(114, 153)+
  ylim(43, 10)+
  theme_void()
print(map)

gg_inset_map <-  ggdraw() +
  draw_plot(map1) +
  draw_plot(map, x = 0.55, y = 0, width = 0.3, height = 0.3)
gg_inset_map

ggsave("Outputs/Maps/insetmap.png", plot = gg_inset_map)
