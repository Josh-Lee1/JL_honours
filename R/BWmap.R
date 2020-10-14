library(tidyverse)
library(raster)
library(rgdal)
library(viridis)
library(sf)
library(tmap)
install.packages("maps")
library(maps)
install.packages("rgeos")
library(rgeos)

###### Precipitation map ########

custom_bins <- c(50, 100, 200, 300, 400, 500, 1000, 1500, 2000, 3000,
                 4500)

r <- raster(nrow=18, ncol=36)
values(r) <- runif(ncell(r)) * 10
r[r>8] <- NA
pol <- rasterToPolygons(r, fun=function(x){x>6}) %>% 
  st_as_sf()

plot(r)

st_as_sf(r)

av_prec <- raster("GIS/precip_final.tif")
  as.data.frame(xy = T) %>% 
  st_as_sf(coords = c("x", "y")) %>%  
  filter(!is.na(precip_final)) 

av_precpol <- rasterToPolygons(av_prec) %>% 
  st_as_sf()

?rasterToPolygons
park_points <- read_csv("GIS/excl_latlong.csv") %>% 
  st_as_sf(coords = c("lon", "lat")) %>%   
  filter(ID == "1")

install.packages("rnaturalearth")
library(rnaturalearth)
australia = ne_countries(country = "Australia") 
class(australia)

map_australia <- st_as_sf(australia)

map_australia <- st_read("GIS/Aus_map/mainlands.shp") %>% 
  st_transform(crs = 4326)

aus_region = st_bbox(c(xmin = 140, xmax = 146,
                      ymin = -34, ymax = -29),
                    crs = st_crs(map_australia)) %>% 
  st_as_sfc()

bbox(park_points)

breaks = c(1, 2, 3, 4, 5) * 100

data("World")



install.packages("shinyjs")
tmaptools::palette_explorer()

site_map <- tm_shape(av_precpol, bbox = aus_region) +
  tm_fill(col = "precip_final", breaks = breaks, 
          title = "AAP", palette = "Blues") +
  tm_legend(legend.position = c("right", "top"), title.size = 2, text.size = 1) +
  tm_shape(park_points) +
  tm_dots(size = 2, shape = "Park", legend.shape.show = F) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 2)

aus_map <- tm_shape(map_australia) + tm_borders() +
  tm_shape(aus_region) + tm_borders(lwd = 3)
aus_map

library(grid)

site_map
print(aus_map, vp = viewport(0.31, 0.19, width = 0.7, height = 0.33))

tmap_save(site_map,
          insets_tm = aus_map, 
          insets_vp = viewport(0.2, 0.19, width = 0.7, height = 0.33), 
          filename = "Output/map_paper.png")

?tmap_save

ggplot() +
  geom_tile(data = av_prec, 
            aes(x = x, y = y, fill=factor(precip_final))) +
  scale_fill_viridis(discrete = T) +
  geom_point(data = park_points, 
             aes(x = lon, y = lat, color = Park)) +
  theme_void()

