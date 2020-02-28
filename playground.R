library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)
library(lubridate)

#Read the wrs tiles 
wrs <- st_read('data/in/wrs2_asc_desc.shp')

lookup_table <- read.delim('data/in/lookup_table.txt')

pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )

global <- reactiveValues(min_date = Sys.Date(), max_date = Sys.Date() + 16)



ui <- fluidPage(
   
   titlePanel("World Reference System Satellite Tiles"),
   mainPanel("Click on a tile to see its next WRS overpass date"),
   leafletOutput('map'),
   actionButton("refreshButton", "Refresh Map"),
   
   fluidRow(
      column(12,
             tableOutput('table'),
      ),
      
      column(12, offset = 8,
             dateRangeInput("dates", "Date range:",
                            start = Sys.Date(),
                            end = Sys.Date()+16),
      ),
      
      column(12, offset = 8, 
             actionButton('applyDates', "Apply Date Filter"),
             ),
   )
   
)



server <- function(input, output){
   
   
   output$map <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% setView(lat=10, lng=0, zoom=2)

      
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
         
         
         #Create a binary matrix of all the polygons that intersect pnt (a user's mapclick)
         #And select those polygons
         boolean_matrix <- st_intersects(wrs, pnt, sparse = FALSE)
         overlapping_tiles <- (wrs[boolean_matrix,])
         overlapping_paths <-  overlapping_tiles$PATH
         overlapping_rows <- overlapping_tiles$ROW
         tile_ID <- overlapping_tiles$PR
         
         
         #Rows of the lookup table from the intersected tile; pull the "Overpass" column (earlier known date of satellite overpass)
         known_pass <- lookup_table[overlapping_paths,]$Overpass
         
         if(input$applyDates){
            
            start_date <- input$dates[1]
            end_date <- input$dates[2]
            days_til <- (as.numeric(start_date - known_pass)) %% 16
            next_pass <- (start_date + days_til) %>% as.Date()
            
            len <- 1:length(next_pass)
            updating_table <- NULL
            
            for (r in len){
               x <- seq.Date(next_pass[r], to = end_date, by = 16) %>% as.character.Date()
               updating_table <- rbind(updating_table, x)
               
            }
            
            output_table <- cbind(tile_ID, overlapping_rows, overlapping_paths, updating_table)
            
            

         }
      
         
         else{
            
            days_til <- (as.numeric(Sys.Date()) - known_pass) %% 16
            next_pass <- (Sys.Date() + days_til) %>% as.character.Date()
            
            #Bug still exists that allows user to click around (intended) to see new tiles, but won't add those tiles to the output table
            output_table <- cbind(tile_ID, overlapping_rows, overlapping_paths, next_pass)
            
         
         }
         
         
         #Generate and display output table
         output$table <- renderTable(output_table)
         
         
         #Update the map: clear all tiles, then add only the ones that overlap in Red
         leafletProxy("map")%>%
            addProviderTiles(providers$Esri.WorldImagery) %>% 
            addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% 
            setView(lng = lon , lat = lat, zoom = 6) %>%
            addPolygons(data = overlapping_tiles, color = 'red')
         
      }  
      
   })
   
   
   #Use a separate observer to refresh the map as needed
   observe( {
      
      proxyMap <- leafletProxy("map")
      
      if(input$refreshButton){
         proxyMap%>%
            clearShapes()%>%
            addProviderTiles(providers$Esri.WorldImagery) %>%
            addProviderTiles(providers$CartoDB.VoyagerOnlyLabels) %>% 
            setView(lat=10, lng=0, zoom=2)

         output$table <- renderTable(NULL)
         
      }
      
   })
   
}




shinyApp(ui, server)

