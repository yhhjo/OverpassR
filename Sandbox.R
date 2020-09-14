library(sf)
library(tidyverse)
library(rgdal)
library(mapview)
library(lubridate)

# Global variables 
SWATHS <- st_read('Data/In/Sentinel/Sentinel-2A_MP_ACQ_KML_20200716T120000_20200803T150000.kml', layer = "NOMINAL")
MGRS <- st_read('Data/In/Sentinel/Relevant_MGRS.shp')

# Recreated Envitonment Variables ---------
start <- today()
stop <- start + 10 
click <- cbind(5, 60) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326

#TODO:
# Intersect swaths with the click
swaths <- SWATHS[st_intersects(click, SWATHS, sparse = FALSE),]
mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]

# Read the Datetime (POSIXct object) from each swath that intersects the user's click. It's stored under swaths$begin.
begin <- swaths$begin

# Find the whole number of days between the swath's reference datetime and the start of the user's selected range (date).

# Modulo that whole number by 5 [sentinel return period], and add the result to the start of the date range to get the first overpass within the user's selected range.
# Add the time from the swaths's reference Datetime to this first overpass within range
# Increment first overpass by 5 days while preserving the time until a datetime is outside of the date range.
# Save all of these 5-day increments as Datetime objects.
# Merge this array of Datetime objects with the array of all MGRS tiles that intersect the click (creates all possible tile-datetime combos since Sentinel acquires images by swath, not by tile).
# Generate the output table.

