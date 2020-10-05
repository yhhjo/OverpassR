library(rgdal)
library(sf)
library(tidyverse)

#For web scraping
library(xml2)
library(rvest)
library(mapview)


update_swaths <- function() {
  
  # Fetch if today is more than 10 days over the latest acquisition SWATH
  if (!(SWATHS$begin %>% as.Date() %>% max() - today()) < -10) {
    return()
  } else {
    
    out <- tryCatch(
      
      {
        # === EXPR block of trycatch function ===
        url <-
          'https://sentinel.esa.int/web/sentinel/missions/sentinel-2/acquisition-plans'
        
        # Pull HTML and CSS source code
        page <- read_html(url)
        
        # Pulls specific attribute within webpage that contains the latest sentinel-2a acquisition swath URL extension
        link <-
          (page %>% html_nodes(".sentinel-2a") %>% html_nodes("a"))[1] %>% as.character()
        
        # Regex: match everything between "/d" and .kml on the hyperlink attribute
        regex <- "/d(.*)\\.kml"
        
        # Extracts the extension
        extension <- regmatches(link, regexpr(regex, link))
        url <- paste0("https://sentinel.esa.int", extension)
        
        # Download kml as temp file
        download.file(url, "temp.kml")
        stopifnot(file.rename("temp.kml", "./Data/In/Sentinel/Swaths.kml"))
      
        # Update local data
        SWATHS <<- st_read("Data/In/Sentinel/Swaths.kml", layer = "NOMINAL")
      },
      
      # === WARNING HANDLING ===
      warning = function(cond) {
        output$helpText <- renderText("A warning flag was raised while updating ESA Sentinel 2 Database. Please report this bug to ajb28@live.unc.edu ")
      },
      
      # === ERROR HANDLING  ===
      error = function(cond) {
        output$helpText <- renderText("An error occurred connecting to ESA Sentinel 2 Database. Please report this bug to ajb28@live.unc.edu ")
      }
    )
  }
}

