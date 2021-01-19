wptsgpx <- readGPX("Data/Raw/waypts.gpx")
waypts<- as.data.frame(wptsgpx$waypoints)
write.csv(waypts, "Data/Raw/waypts.csv")
