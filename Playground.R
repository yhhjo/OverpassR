library(leaflet)
library(tidyverse)
library(plyr)
library(dplyr)
library(mapview)
library(rgdal)
library(sf)

#Global Variable
PARSED <- st_read('Data/In/Sentinel/s2_swaths.shp', stringsAsFactors = FALSE)
st_crs(PARSED) = 4326
MGRS <- st_read('Data/In/Sentinel/sentinel2_tiles_world.shp', stringsAsFactors = FALSE)

#Reactive Data
lon <- 73.1
lat <- 67.1
start <- "2020-07-01" %>% as.Date() %>% as.numeric()
stop <- "2020-07-13" %>% as.Date()

#------------------------

click <- cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326
swaths <- PARSED[st_intersects(click, PARSED, sparse = FALSE),]
mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]

output_table <- data.frame()

#For each MGRS tile
for (r in 1:length(mgrs)){
  
  swath_dates <- swaths[st_contains(swaths, mgrs[r,], sparse = FALSE),]$Overpass %>% as.Date() %>% as.numeric()
  first_pass_within_range <- start + ((start - swath_dates) %% 5)
  
  #Eliminates duplicates
  first_pass_within_range <- first_pass_within_range[!(first_pass_within_range %% 5) %>% duplicated()]
  
  for (c in 1:length(first_pass_within_range)){
    from <- first_pass_within_range[c] %>% as.Date('1970-01-01')
    dates <- seq.Date(from, stop, by = 5) %>% as.character()
    updating <- cbind("Dates" = dates, "MGRS" = rep(mgrs[r,]$Name, length(dates)),"WRS" = NA  ,"Satellite" = rep("Sentinel2", length(dates)), 
                      "Lat" = lat, "Lon" = lon)

    output_table <- rbind(output_table, updating)
  }

}

output_table <- output_table %>% distinct()
output_table

