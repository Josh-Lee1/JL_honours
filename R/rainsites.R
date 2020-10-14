library(raster)
library(sf)


#Loading Data

fesm <- raster("fesm1000.tif")
rain200 <- raster("rain200.tif")
## raster of all NSW rainforest at 200m res, I have one at 5m too but it keeps crashing my computer


# Converting cells to pts
rfor200 <-  rasterToPoints(rain200)

##This step looks like it did what I wanted but I'm not sure what the crs is?

# Trim points to shapefile
fireshape<- st_read("National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211.shp")

rain_pts <- st_as_sf(rain200, coords=c("rfor200", "y"), crs=st_crs(fireshape))
pointsinout<-st_join(rain_pts, fireshape, join = st_within)

#Or something like this?
#pointsin <- fireshape %>% st_join(rain_pts) %>% 
#  st_set_geometry(NULL) %>% 
#  mutate(in_fire="True")

#pointsinout <- rain200 %>%
#  left_join(., pointsin)

# Next I wanted to organise by the "ClassName" which distinguished the rainforest types in the original raster
# however I'm not sure how to view cell values of rasters in r.

# Place these points on fesm raster and assign severity values.

# The end goal would be to do stratified sampling randomly selecting a proportional number of points
# in each rainforest type across fire severity values