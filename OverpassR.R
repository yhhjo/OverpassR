#All packages used at some point in the project - will narrow down at the end
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)
library(lubridate)
library(DT)
library(dplyr)
library(plyr)

#Read the wrs tiles 
wrs <- st_read('data/in/wrs2_asc_desc.shp')
wrs <- wrs[wrs$MODE == 'D',]

#Lookup table and array of dates
lookup_table <- read.delim('data/in/lookup_table.txt')

#Global variable output table. It will update with each map click and reset only when 'reset map' button is clicked
global_table = data.frame("tile_ID" = numeric(), "overlapping_rows" = numeric(), "overlapping_paths" = numeric(), "next_pass" = character()) %>% t() %>% as.data.frame()

ui <- fluidPage(
  
  titlePanel("World Reference System Satellite Tiles"),
  mainPanel("Click on a tile to see its next WRS overpass date"),
  leafletOutput('map'),
  actionButton("refreshButton", "Clear Tiles"),
  
  fluidRow(
    column(12, DTOutput('table')
    ),
    
    column(12, offset = 8,
           dateRangeInput("dates", "Date range:",
                          start = Sys.Date(),
                          end = Sys.Date()+16)
    ),
    
    column(12, offset = 8, 
           actionButton('applyDates', "Apply Date Filter")#,
    )
  )  
)



server <- function(input, output){
  
  #Render map with base layers WorldImagery and Labels
  output$map <- renderLeaflet({

    leaflet() %>%
      addTiles(group = "Default") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
      setView(lat=10, lng=0, zoom=2)  %>%
      addLayersControl(
        baseGroups = c("Default", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% 
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% setView(lat=10, lng=0, zoom=2) 
  })
  
  
  proxy_table = dataTableProxy('table')
  
  
  observeEvent(input$map_click,{
    
    click <- input$map_click
    lat <- click$lat
    lon <- click$lng
    
    if(is.null(click)) 
      return() 
    
    #Pull the latitude, longitude, and path row (pr) from click event
    else {
      
      #Save the coordinates as a data frame, and convert it to a spatial point
      coords <- as.data.frame(cbind(lon, lat)) 
      point <- SpatialPoints(coords)
      proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      #Convert the point to a shape file so they can intersect
      pnt <-  st_as_sf(point, coords = c('lon' , 'lat'))
      
      
      #Create a boolean matrix of all the polygons that intersect pnt (a user's mapclick)
      ## And select those polygons from WRS
      boolean_matrix <- st_intersects(wrs, pnt, sparse = FALSE)
      overlapping_tiles <- (wrs[boolean_matrix,])
      overlapping_paths <-  overlapping_tiles$PATH
      overlapping_rows <- overlapping_tiles$ROW
      tile_ID <- overlapping_tiles$PR
      
      
      #Rows of the lookup table from the intersected tile; 
      ##pull the "Overpass" column (earlier known date of satellite overpass)
      known_pass <- lookup_table[overlapping_paths,]$Overpass
      
      
      if(input$applyDates){
        
        start_date <- input$dates[1]
        end_date <- input$dates[2]
        days_til <- (as.numeric(start_date - known_pass)) %% 16
        next_pass <- (start_date + days_til) %>% as.Date()
        
        len <- 1:length(next_pass)
        updating_table <- NULL
        
        for (r in len){
          x <- seq.Date(next_pass[r], to = end_date, by = 16) %>% as.data.frame.character()
          updating_table <- cbind.pad(updating_table, x)
        }
        
        updating_table <- updating_table %>% t() %>% t()
        
        appending_table <- rbind("tile_ID" = tile_ID, "overlapping rows" = overlapping_rows, 
                                 "overlapping paths" = overlapping_paths,"next pass" = updating_table) %>% as.data.frame()
        appending_table <- `colnames<-`(appending_table, 'Tile')
        
        #If global table isn't empty, update it with the new click and unique values
        if(!is.null(global_table)){
          global_table <<- cbind.pad(appending_table, global_table) %>% as.data.frame()
        }
        
        else
          global_table <<- appending_table     
        
      }
      
      
      else{
        
        days_til <- (as.numeric(Sys.Date()) - known_pass) %% 16
        next_pass <- (Sys.Date() + days_til) %>% as.character.Date()
        
        #Table with all of the data from the map click
        appending_table <- rbind("tile_ID" = tile_ID, "overlapping_rows" = overlapping_rows,
                                 "overlapping_paths" = overlapping_paths, "next_pass" = next_pass) %>% as.data.frame()
        appending_table <- `colnames<-`(appending_table, "Tile")
        
        #If global table is already populated , update it with appending-table
        if(!is.null(global_table))
          global_table <<- cbind.pad(appending_table, global_table)
        
        
        else
          global_table <<- appending_table
        
      }
      
      #Generate and display output table
      output$table <<- renderDT(
        global_table, options = list(searching = FALSE)
        )
    
      #Update the map: clear all tiles, then add only the ones that overlap in Red
      leafletProxy("map") %>%
        addTiles(group = "Default") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
        setView(lng = lon , lat = lat, zoom = 6) %>%
        addPolygons(data = overlapping_tiles, color = 'red') %>%
        addLayersControl(
          baseGroups = c("Default", "Satellite"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }  
    
  })
  
  
  #Use a separate observer to clear shapes and output table if "clear tiles" button clicked
  observe({
    
    proxyMap <- leafletProxy("map")
    
    if(input$refreshButton){
      
      proxyMap%>%
        clearShapes()
      
      output$table <- renderDT(NULL)
      global_table <<- NULL
      
    }
  })
}





#Helper Functions


#Given two data frames of different length, return a third array of x amd mx merged by columns
#with NULL filling the empty space 
cbind.pad <- function(x, mx){
  
  
  #Base cases: passing one or two null values, equal dimensions
  if(is.null(x) && is.null(mx))
    return(data.frame())
  
  else if(is.null(x))
    return(mx)
  
  else if(is.null(mx))
    return(x)
  
  len <- max(nrow(x), nrow(mx))
  
  
  if(nrow(x) == nrow(mx))
    return(cbind(x, mx))
  
  #Real work of the method: 
  else if(nrow(x) < nrow(mx)){
    x <- padNULL(x, len)
    return(cbind(x, mx))
    
    
  }
  
  else{
    mx <- padNULL(mx, len)
    return(cbind(x, mx))
  }
  
}

#Recursive function that adds NULLs to the end of data frame x until x reaches the desired len
padNULL <- function(x, len){
  
  if(is_empty(x))
    return(data.frame(rep(NA, len)))
  
  if(nrow(x) == len)
    return(x)
  
  else
    return(padNULL(rbind(x, rep(NA, ncol(x))), len))
  
}

shinyApp(ui, server)