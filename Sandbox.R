library(sf)
library(tidyverse)
library(rgdal)
library(mapview)

SWATHS <- st_read('Data/In/Sentinel/Sentinel-2A_MP_ACQ_KML_20200716T120000_20200803T150000.kml', layer = "NOMINAL")
SWATHS$Overpass <- as.Date(SWATHS$begin, format = "%y/%m/%d")
as_datetime(SWATHS$begin)
begin <- SWATHS$begin
times <- strftime(begin, format = "%H:%M")

##TODO

