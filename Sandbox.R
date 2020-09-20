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
lat <- 60
lon <- 5


# Setting local variables
click <- cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
st_crs(click) = 4326
swaths <- SWATHS[st_intersects(click, SWATHS, sparse = FALSE),]
mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]

# Read the Datetime as lunridate ymd_hms object from each swath that intersects the user's click
begin <- swaths$begin %>% ymd_hms()

# Find the whole number of days between the swath's reference datetime and the start of the user's selected range (date).
daterange_offset <- (difftime(begin, start) %>% as.integer() %% 5) %>% ddays()

# Datetime objects within user selected range
dates <-  start + daterange_offset
times <- strftime(begin, format = "%H:%M:%S", tz = "UTC") %>% as.character.Date()
datetimes <- data.frame()

# Loops roughly the  
for(i in 1:length(dates)) {
  x <- seq.Date(dates[i], stop, 5)
  x <- data.frame("Date" = x[x <= stop]) %>% as.character.Date()
  datetimes <- rbind(datetimes, merge(x, times[i]))
}
names(datetimes) <-  c("Date", "Time")

output <- datetimes %>% as.character.Date() %>% merge(mgrs$Name)
names(output) <- c("Date", "Time (UTC)" , "MGRS")
output <- cbind(output, "Path" = NA, "Row" = NA, "Lat" = lat, "Lon" = lon, "Satellite" = "Sentinel2") %>% unique()

# END ALGORITHM
