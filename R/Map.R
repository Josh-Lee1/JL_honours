library(sf)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)
library(ggsn)
library(ggspatial)


#prepare df
birds <- read.csv("Data/Raw/birds.csv")
birds$Treatment<- with(birds, paste0(Formation, Fire))
siteinfo <- dplyr::select(birds, "Site", "Longitude", "Latitude", "Elevation", "Location", "Formation", "Fire", "Treatment")

#prepare map
Aus<- st_read("Data/Raw/1259030001_ste11aaust_shape/STE11aAust.shp")
sites <- st_as_sf(siteinfo, coords = c("Longitude", "Latitude"), crs = st_crs(4326))
wha <- st_read("Data/Raw/world_heritage_public/world_heritage_public.shp") %>% 
  filter(NAME == "Gondwana Rainforests of Australia")
fire <- st_read("Data/Raw/NIAFED_v20200211/National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211.shp")

#make map
map <- ggplot() +
  geom_sf(data=Aus) +
  geom_sf(data=sites, aes(colour = sites$Treatment))+
  xlim(140, 154)+
  ylim(37, 28)
print(map)

#better map
##study area with layers
map1 <- ggplot() +
  geom_sf(data = Aus, fill = "#FFFFCC") +
  geom_sf(data = wha, fill = NA, aes(colour = "A"), size = 1, show.legend = "line") +
  geom_sf(data = fire, colour = alpha("red", 0.2), aes(colour = "B"), size = 1, show.legend = "line") +
  geom_sf(data=sites, aes(shape = sites$Formation, colour = sites$Fire)) +
  xlim(152.2, 153.7)+
  ylim(29.5, 28.2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("A" = "#009900",
                                "B" = "#FF0000",
                                "#330000",
                                "#3399FF",
                                "#FF6600",
                                "#0000FF"),
                     breaks = c("A", "B",
                                "Dry SclerophyllBurnt",
                                "Dry SclerophyllUnburnt",
                                "RainforestBurnt",
                                "RainforestUnburnt"),
                     labels = c("World Heritage Area",
                                "2019-20 Bushfire",
                                "Burnt Dry Sclerophyll",
                                "Unburnt Dry Sclerophyll",
                                "Burnt Rainforest",
                                "Unburnt Rainforest"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid",
                                    "solid",
                                    "blank",
                                    "blank",
                                    "blank",
                                    "blank")),
                       shape = c(NA, NA, 16, 16, 16,16))) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",  
                         pad_y = unit(0.5, "in"), 
                         style = north_arrow_orienteering())+
  annotation_scale(location = "tr", width_hint = 0.25, style = "ticks") +
  theme(legend.title = element_blank(),
        legend.key = element_rect(fill = "#FFFFCC", color = NA))
  
print(map1)

#still have to shade in fire

#define study area (redbox)
study_area <- st_as_sfc(st_bbox(sites))

##Make inset
map <- ggplot() +
  geom_sf(data=Aus, fill = "white") +
  geom_sf(data = study_area, fill = "red", color = "red", size = 2) +
  xlim(114, 153)+
  ylim(43, 10)+
  theme_void()
print(map)

##combine map and inset
gg_inset_map <-  ggdraw() +
  draw_plot(map1) +
  draw_plot(map, x = 0.66, y = 0.26, width = 0.2, height = 0.2)
print(gg_inset_map)

ggsave("Outputs/Maps/insetmap.png", plot = gg_inset_map)



#more tests



mapx <- ggplot() +
  geom_sf(data = Aus, fill = "#FFFFCC") +
  geom_sf(data = fire, fill = alpha("#FF0000", 0.5), colour = "Red", size = 1) +
  geom_sf(data = wha, fill = alpha("#009900", 0.5), colour = "Green", size = 1) +
  scale_fill_manual(values = "red", 0.2)+
  geom_sf(data=sites, aes(shape = sites$Formation, colour = sites$Fire)) +
  scale_colour_manual(values = c("#FF33FF", "#00CCFF")) +
  xlim(152.2, 153.7)+
  ylim(29.5, 28.2) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  theme(legend.position = "none") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",  
                         pad_y = unit(0.5, "in"), 
                         style = north_arrow_orienteering())+
  annotation_scale(location = "tr", width_hint = 0.25, style = "ticks")

print(mapx)

##combine map and inset
gg_inset_map <-  ggdraw() +
  draw_plot(mapx) +
  draw_plot(map, x = 0.73, y = 0.1, width = 0.2, height = 0.2)
print(gg_inset_map)

ggsave("Outputs/Maps/insetmap1.png", plot = gg_inset_map)
