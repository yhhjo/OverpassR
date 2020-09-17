library(shiny)
library(sf)
library(tidyverse)
library(rgdal)
library(mapview)
library(lubridate)

# Global variables 
SWATHS <- st_read('Data/In/Sentinel/Sentinel-2A_MP_ACQ_KML_20200716T120000_20200803T150000.kml', layer = "NOMINAL")
MGRS <- st_read('Data/In/Sentinel/Relevant_MGRS.shp')

# Recreated Environment Variables ---------
start <- today()
stop <- start + 30 
click <- cbind(5, 60) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326

#TODO:
# Intersect swaths with the click
swaths <- SWATHS[st_intersects(click, SWATHS, sparse = FALSE),]
mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]

# Read the Datetime (POSIXct object) from each swath that intersects the user's click. It's stored under swaths$begin.
begin <- swaths$begin %>% ymd_hms()

# Find the whole number of days between the swath's reference datetime and the start of the user's selected range (date).
daterange_offset <- (difftime(begin, start) %>% as.integer() %% 5) %>% ddays()

# Datetime objects within user selected range
dates <-  start + daterange_offset
times <- strftime(begin, format = "%H:%M:%S", tz = "UTC") %>% hms()
overpass <- data.frame("Overpass" = NULL)

# Loops roughly the  
for(i in 1:length(dates)) {
  x <- seq.Date(dates[i], stop, 5) + times[i]
  x <- data.frame("Overpass" = x[x <= stop])
  overpass <- rbind(overpass, x)
}


#TODO: 
# Generate the output table.
