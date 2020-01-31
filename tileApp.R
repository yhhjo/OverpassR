library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)

#Read the wrs tiles 
wrs <- 
  st_read('data/in/wrs2_asc_desc.shp')%>%
  sample_frac(.05)


pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )
test <- leaflet(wrs) %>% addTiles() %>% addPolygons(stroke = TRUE, color = ~pal(PATH))




ui <- fluidPage(
  
  titlePanel("World Reference System Satellite Tiles"),
  leafletOutput('map')
  
  )
  


server <- function(input, output){
  
  filteredData <- reactive (input$map_shape_click)
  
  
  observeEvent(input$map_shape_click,{
    
    #click variable stores lat, long, and .nonce of the selected tile
    click <- input$map_shape_click
    
    if(is.null(click))
      return()
    
    #Pull the latitude and longitude from click event
    lat <- click$lat
    lon <- click$lng
    
    #print( (lat == click$lat) )
    
    #put lat lon from the click point into their own data frame
    cords <- as.data.frame(cbind(lon, lat))
    
    #convert click point coords into a spatial point (SP) and sets CRS
    point <- SpatialPoints(cords)
    proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    
    ##THIS IS WHERE THINGS GET WEIRD
    
    
    #Retrieve the polygons in which the click point resides
    pnts <-  st_as_sf(point, coords = c('lon' , 'lat'))
    intersection <-  as.array(st_intersects(pnts, wrs, sparse = FALSE))

    print("\n\nINTERSECTION:")
    print(intersection)

    
  }
    
  )  
  output$map <- renderLeaflet(
    test
  )
  

          
}
    
  


shinyApp(ui, server)

