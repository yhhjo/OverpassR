library(leaflet)
library(tidyverse)
library(plyr)
library(dplyr)
library(mapview)
library(rgdal)
library(sf)
library(lwgeom)

#Global Variable
PARSED <- st_read('Data/In/Sentinel/s2_swaths.shp', stringsAsFactors = FALSE)
st_crs(PARSED) = 4326
MGRS <- st_read('Data/In/Sentinel/sentinel2_tiles_world.shp', stringsAsFactors = FALSE)

#Reactive Data
lon <- 56.76
lat <- 47.22
start <- "2020-07-01" %>% as.Date() %>% as.numeric()
stop <- "2020-07-13" %>% as.Date()

#------------------------


click <- cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326

#Swaths that contain the click
swaths <- PARSED[st_intersects(click, PARSED, sparse = FALSE),]
mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]

if(is_empty(swaths$Overpass)){
  return()
}

output_table <- data.frame()


#For each MGRS tile, find its date
for (r in 1:length(mgrs)){
  
  #Assures mgrs tile is fully within swath, not just overlapping
  #swath_dates <- swaths[st_contains(swaths, mgrs[r,], sparse = FALSE),]$Overpass %>% as.Date() %>% as.numeric()
  
  #Swaths that contain 90% of tile  
  bool <- ((st_intersection(swaths, mgrs[r,]) %>% st_area()  %>% as.numeric() ) / (st_area(mgrs[r,]) %>% as.numeric())) >.9
  swath_dates <- swaths[bool,]$Overpass %>% as.Date() %>% as.numeric()
  
  first_pass_within_range <- start + ((start - swath_dates) %% 5)
  #Eliminates duplicates
  first_pass_within_range <- first_pass_within_range[!(first_pass_within_range %% 5) %>% duplicated()]
  
  #For each distinct overpass for mgrs tile (5 or fewer day difference)
  for (c in 1:length(first_pass_within_range)){
    
    from <- first_pass_within_range[c] %>% as.Date('1970-01-01')
    dates <- seq.Date(from, stop, by = 5) %>% as.character()
    output_table <- rbind(output_table, cbind("Dates" = dates, "Path" = NA, "Row" = NA, "MGRS" = mgrs[r,]$Name,
                      "Lat" = lat, "Lon" = lon, "Satellite" = rep("Sentinel2", length(dates)))) 

  }

}

output_table <- output_table %>% distinct()

output_table

mapview(mgrs) + mapview(click) + mapview(swaths)

