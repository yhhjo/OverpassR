library(rgdal)
library(sf)
library(tidyverse)

#For web scraping
library(xml2)
library(rvest)

url <- 'https://sentinel.esa.int/web/sentinel/missions/sentinel-2/acquisition-plans'

# Pull HTML and CSS source code
page <- read_html(url)

# Pulls specific attribute within webpage that contains the latest sentinel-2a acquisition swath URL extension
link <- (page %>% html_nodes(".sentinel-2a") %>% html_nodes("a"))[1] %>% as.character()

# Regex: match everything between "/d" and .kml on the hyperlink attribute
extension <- regmatches(link, regexpr(regex, link))

# Full URL for the latest ESA Sentinel 2A acquisition swath
retrieve_url <- paste0("https://sentinel.esa.int", extension)


#TODO: Download KML file from retrieve_url, name it something generic 
## TODO: Handle any link download errors 

#TODO: Ensure downloaded file works and isn't corrupted
## TODO: can open st_read, make sure it has layer "NOMINAL", and has at least 50 attributes

#TODO: Replace the old file wuth new one


