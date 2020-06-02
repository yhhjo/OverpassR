library(shiny)
library(leaflet)
library(tidyverse)
library(plyr)
library(dplyr)
library(mapview)
library(rgdal)

parsed <- st_read('Data/In/Sentinel/s2_swaths.shp')
st_crs(parsed) = 4326

lon <- 10
lat <- 10


click <- cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326
bool <- st_intersects(click, parsed, sparse = FALSE)

#Why does this have 19 levels?
swaths <- parsed[bool,]$Overpass
