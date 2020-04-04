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
global_table = data.frame()

##COME BACK HERE
ui <- fluidPage(
  
  fixedRow(
    column(4, offset = 5,
           titlePanel("OverpassR")),
  ),
  
  fixedRow(
    column(10, offset = 3, 
           mainPanel("Click on map or enter coordinates to view satellite overpass information")
    )
  ),
  
  leafletOutput('map', width = 1000, height = 500),
  
  fluidRow(
    column(1, offset = 0, actionButton("refreshButton", "Reset")),
    uiOutput("ui")
  ),
  
  #Row with coordinate input, date input, search
  fixedRow(
    
    column(2, offset = 1,
           textInput("lat", label = NULL , value = "", placeholder = "Lat", width = 130)
    ),
    column(2,
           textInput("lon", label = NULL , value = "", placeholder = "Lon", width = 130)
    ),
    
    column(1, offset = 0, 
           actionButton("find", label = "Find")
    ),
    
    column(3, offset = 2,
           dateRangeInput("dates", "Date range:",
                          start = Sys.Date(),
                          end = Sys.Date()+16)
    )
  ),
  
  #Apply Date Button
  fixedRow( 
    column(1, offset = 8,
           actionButton('applyDates', "Apply Date Filter")
    )
  ),
  
  fluidRow(
    column(8, offset = 1, DTOutput('table'))
  )
)



server <- function(input, output, session){
  
  proxy_table = dataTableProxy('table')
  proxyMap <- leafletProxy("map")
  
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
    
    
  })
  
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
       
        #Validate is preventing the app running dates out of range, but no error message is being displayed, 
        validate(
          need( (as.numeric(input$dates[2] - input$dates[1]) >=16), label = "ui", message =  "Please enter valid date range") 
        )
    
        start_date <- input$dates[1]
        end_date <- input$dates[2]
        
        
        
        days_til <- (as.numeric(start_date - known_pass)) %% 16
        next_pass <- (start_date + days_til) %>% as.Date()
        
        len <- 1:length(next_pass)
        updating_table <- NULL
        
        #Each loop iteration creates a data frame "temp" of dates, path, row, lat, lon for one tile
        ## Subseqeuent iterations create a temp data frame, then append the new data frame to the previous one
        for (r in len){
          dates <- seq.Date(next_pass[r], to = end_date, by = 16) %>% as.character()
          temp <- cbind("Date" = dates, "Path" = overlapping_paths, "Row" = overlapping_rows, "Lat" = lat, "Long" = lon)
          updating_table <- rbind(updating_table, temp)
        }
        
        
        #If global table isn't empty, update it with the new click and unique values
        if(!is.null(global_table)){
          global_table <<- rbind(updating_table, global_table)
        }
        
        else
          global_table <<- updating_table     
        
      }
      
      
      else{
        
        days_til <- (as.numeric(Sys.Date()) - known_pass) %% 16
        next_pass <- (Sys.Date() + days_til) %>% as.character.Date()
        
        #Table with all of the data from the map click
        appending_table <-  cbind("Date" = next_pass, "Path" = overlapping_paths, 
                                  "Row" = overlapping_rows, "Lat" = lat, "Long" = lon) %>% as.data.frame()

        #If global table is already populated , update it with appending-table
        if(!is.null(global_table))
          global_table <<- rbind(appending_table, global_table)
        
        
        else
          global_table <<- appending_table
        
      }
      
      #Generate and display output table
      output$table <<- renderDT(
        global_table, options = list(paging = FALSE, searching = FALSE, info = FALSE) 
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
    

    if(input$refreshButton){
      
      proxyMap%>% clearShapes()
      
      updateDateRangeInput(session, "dates", "Date range:",
                           start = Sys.Date(),
                           end = Sys.Date()+16
                           )
    
      output$table <- renderDT(NULL)
      global_table <<- NULL
      
    }
  })
  
  #Observer if Lat/Lon are manually entered
  observe({
    
    if(input$find){
      lat <- input$lat %>% as.numeric()
      lon <- input$lon %>% as.numeric()
      
      validateCoords(lon, lat)
    }
    
  })
}







shinyApp(ui, server)
