library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)
library(lubridate)

#Read the wrs tiles 
wrs <- 
  st_read('data/in/wrs2_asc_desc.shp')%>%
  sample_frac(.10)

lookup_table <- read.delim('data/in/lookup_table.txt')

pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )


ui <- fluidPage(
  
  titlePanel("World Reference System Satellite Tiles"),
  leafletOutput('map'),
  actionButton("button", "Refresh Map"),
  fluidRow(
    column(12,
           tableOutput('table')
    )
  )
  
)



server <- function(input, output){
  
  
  output$map <- renderLeaflet({
    
    leaflet("map", data = wrs)%>%
      addTiles()%>%
      setView(lat=10, lng=0, zoom=2) %>% 
      addPolygons(color = ~pal(PATH), layerId = ~PR, 
                  highlightOptions = highlightOptions(color ='Red',
                                                      opacity = 0.7))
    
    
  })
  
  
  observeEvent(input$map_shape_click,{
    
    click <- input$map_shape_click
    lat <- click$lat
    lon <- click$lng
    pr <- click$id
    
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
      mat <- st_intersects(wrs, pnt, sparse = FALSE)
      overlappingTiles <- (wrs[mat,])
      paths <-  overlappingTiles$PATH
      
      #Rows of the lookup table from the intersected tile; pull the "Overpass" column (earlier known date of satellite overpass)
      known_passes <- lookup_table[paths,]$Overpass
      
      days_til <- (as.numeric(Sys.Date()) - known_passes) %% 16
      next_pass <- (Sys.Date() + days_til) %>% as.character.Date()
      
      #Generate an output of the next pass and PR id of the tile (in case multiple tiles selected)
      output_table <- cbind(pr, next_pass) 
      
      
      output$table <- renderTable(output_table)
      
      
      #Update the map: clear all tiles, then add only the ones that overlap in Red
      leafletProxy("map")%>%
        clearShapes()%>%
        addTiles%>%
        setView(lng = lon , lat = lat, zoom = 5) %>%
        addPolygons(data = overlappingTiles, color = 'red')
      
    }  
    
  })
  
  #Use a separate observer to refresh the map as needed
  observe( {
    
    proxyMap <- leafletProxy("map")
    
    if(input$button){
      proxyMap%>%
        clearShapes()%>%
        addTiles%>%
        setView(lng = 0 , lat = 10, zoom = 2) %>%
        addPolygons(data = wrs, color = ~pal(PATH), layerId = ~PR, 
                    highlightOptions = highlightOptions(color ='Red',
                                                        opacity = 0.7))
      
      output$table <- renderTable(NULL)
      
    }
    
  })
  
}




shinyApp(ui, server)

