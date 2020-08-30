library(rgdal)
library(sf)
library(tidyverse)

#TODO: Fetch swaths and save as kml using Regex search 

#Read kml file: st_read(swaths, layer = nominal)
swaths <- st_read('Data/In/Sentinel/Sentinel-2A_MP_ACQ_KML_20200716T120000_20200803T150000.kml', layer = "NOMINAL")
swaths$Overpass <- as.Date(swaths$begin, format = "%y/%m/%d")
swaths$Times <- strftime(swaths$begin, format = "%H:%M")

#Read MGRS file. Write "Overpass" and "Time" columns as <NA>
mgrs <- st_read('Data/In/Sentinel/Relevant_MGRS.shp')
mgrs$Overpass <- NA
mgrs$Time <- NA

#TODO Clear the unrelated columns

#For each MGRS tile, find the swath(s) it intersects
 ## if one, pull the Overpass and Time info
## If multiple, find the one it overlaps with most 
edges <- data.frame("Rows" = NA)

for (r in 1:length(mgrs$Name)) { 
  swath <- swaths[st_intersects(mgrs[r,], swaths)[[1]],]
  
  
  if(length(swath$name) > 1 ) {
    #TODO: compare areas to find the tile that overlaps most
    break
    }
}
#bool <- ((st_intersection(swaths[1,], mgrs[r,]) %>% st_area()  %>% as.numeric() ) / (st_area(mgrs[r,]) %>% as.numeric())) >.9