library(rgdal)
library(sf)
library(tidyverse)


#TODO: Fetch swaths and save as kml using Regex search 

#Read kml
swaths <- st_read('Data/In/Sentinel/Sentinel-2A_MP_ACQ_KML_20200716T120000_20200803T150000.kml', layer = "NOMINAL")

#Columns to be merged with MGRS
swaths$Overpass <- as.Date(swaths$begin, format = "%y/%m/%d")
swaths$Time <- strftime(swaths$begin, format = "%H:%M")

#Read MGRS file. Write "Overpass" and "Time" columns as <NA>
mgrs <- st_read('Data/In/Sentinel/Relevant_MGRS.shp')
mgrs$Overpass <- NA
mgrs$Time <- NA

#TODO Clear the unrelated columns for faster processing

#For each MGRS tile, find the Swath it most overlaps with, and update "Overpass" and "Time" data from it


#SLOW! Better way of merging data than a for-loop?
for (r in 1:length(mgrs$Name)) { 
  swath <- swaths[st_intersects(mgrs[r,], swaths)[[1]],]
  
  #Tile overlaps with multiple swaths
  if(length(swath$name) > 1 ) {
    #TODO: compare areas to find set swath to one with max overlap.
    mgrs[r,]$Overpass <- NA
    mgrs[r,]$Time <- NA
  }
  
  mgrs[r,]$Overpass <- swath$Overpass
  mgrs[r,]$Time <- swath$Time
}
