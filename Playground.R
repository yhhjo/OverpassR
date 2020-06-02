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
stop <- "2020-07-13" %>% as.Date() %>% as.numeric()


click <- cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326
swath_bool <- st_intersects(click, PARSED, sparse = FALSE)
mgrs_bool <- st_intersects(click, MGRS, sparse = FALSE)

ref_date <- PARSED[swath_bool,]$Overpass %>% as.Date() %>% as.numeric()

dates <- data.frame()

for (r in 1:length(ref_date)){
  first_pass <-  start + ((start - ref_date[r]) %% 5)
  overpasses <- seq.Date( as.Date(first_pass, '1970-01-01'), as.Date(stop, '1970-01-01'), by = 5) %>% as.data.frame()
  dates <- rbind(dates, as.character.Date(overpasses)) %>% distinct()
}

names(dates) <- "Dates"
