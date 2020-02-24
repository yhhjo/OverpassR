library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)
library(sf)

<<<<<<< HEAD
#Read the wrs tiles 
wrs <- 
  st_read('data/in/wrs2_asc_desc.shp')%>%
  sample_frac(.05)


pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )
test <- leaflet(wrs) %>% addTiles() %>% addPolygons(stroke = TRUE, color = ~pal(PATH))


ui <- fluidPage(
  
  
  
  titlePanel("World Reference System Satellite Tiles"),
  leafletOutput('mymap'),
  
  absolutePanel(top = 10, right = 10, fixed = TRUE,
                tags$div(style = "opacity: 0.70; background: #FFFFEE; padding: 8px; ", 
                         helpText("Welcome to WRS Overpass Finder"),
                         textOutput("text") # to display the lat, long and name in absolute panel when shape is clicked
                )
  )
  
  )
=======
  #Read the wrs tiles 
  wrs <- 
    st_read('data/in/wrs2_asc_desc.shp')%>%
    sample_frac(.05)
  
  
  pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )
  
  test <- leaflet(wrs) %>% 
    addTiles() %>% 
    addPolygons(stroke = TRUE, color = ~pal(PATH), layerId = ~PR )
  
>>>>>>> 5a70e258ed237d3143a395abf97f81b0f1759564
  

## R Shiny server code begins here
server <- function(input, output, session) {
  
<<<<<<< HEAD
  output$mymap <- renderLeaflet({
    # Create the map data and add ploygons to it
    leaflet(data=wrs) %>% 
      addTiles() %>% 
      setView(lat=10, lng=0, zoom=2) %>% 
      addPolygons(fillColor = "green",
                  highlight = highlightOptions(weight = 5,
                                               color = "red",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  #label = PR,
                  layerId = ~PR) # add a layer ID to each shape. This will be used to identify the shape clicked
    
  })
  
  # Zoom and set the view after click on state shape
  # input$mymap_shape_click will be NULL when not clicked initially to any shape
  # input$mymap_shape_click will have the ID, lat and lng corresponding to the shape clicked
  # Observe to update the view and zoom level when a shape(country in this case) is clicked
  observe(
    {  click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    pr = click$id
    lat <- click$lat
    lng <- click$lng

    if(is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng , lat = lat, zoom = 5) %>%
      clearMarkers() %>%
      addMarkers(lng =lng , lat = lat, popup = pr)
    # using lat long from spdf will not change the view on multiple clicks on the same shape

    }
  )
  
  # Observe to display click data when a shape (country in this case) is clicked
  observe(
    {click = input$mymap_shape_click
    print(click$lng)
    pr = click$id
    lat <- click$lat
    lng <- click$lng
    if(is.null(click))
      return()
    else
      output$text <- renderText({paste("Latitude= ", lat,
                                       "Longitude=", lng,
                                       "Path/Row=", pr
      )})}

  )
  
}

#
#
# server <- function(input, output){
#
#
#
#   filteredData <- reactive(input$map_shape_click)
#
#   observeEvent(input$map_shape_click,{
#
#     #click variable stores lat, long, and .nonce of the selected tile
#     click <- input$map_shape_click
#
#     if(is.null(click))
#       return()
#
#     #Pull the latitude and longitude from click event
#     lat <- click$lat
#     lon <- click$lgn
#
#     #print( (lat == click$lat) )
#
#     #put lat lon from the click point into their own data frame
#     cords <- as.data.frame(cbind(lng, lat))
#
#     #convert click point coords into a spatial point (SP) and sets CRS
#     point <- SpatialPoints(cords)
#     proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#
#
#     ##THIS IS WHERE THINGS GET WEIRD
#
#
#     #Retrieve the polygons in which the click point resides
#     pnts <-  st_as_sf(point, coords = c('lng' , 'lat'))
#     intersection <-  as.array(st_intersects(pnts, wrs, sparse = FALSE))
#
#     print("\n\nINTERSECTION:")
#     print(intersection)
#
#
#   }
#
#   )
#   output$map <- renderLeaflet(
#     test
#   )
#
#
#
# }

=======
  
  ui <- fluidPage(
    
    titlePanel("World Reference System Satellite Tiles"),
    leafletOutput('map'),
    actionButton("button", "Refresh Map"),
    
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
      latitude <- click$lat
      longitude <- click$lng
      pr <- click$id
      
      if(is.null(click)) 
        return() 
      
      #Pull the latitude, longitude, and path row (pr) from click event
      else {
      
      leafletProxy("map")%>%
        clearShapes()%>%
        addTiles%>%
        setView(lng = longitude , lat = latitude, zoom = 5) %>%
        addPolygons(data = wrs[wrs$PR==pr,], color = 'red')
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
        
      }
      
      })
          
}
    
>>>>>>> 5a70e258ed237d3143a395abf97f81b0f1759564



shinyApp(ui, server)

