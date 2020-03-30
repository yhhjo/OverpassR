library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)
library(sf)

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


## R Shiny server code begins here
server <- function(input, output, session) {
  
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

shinyApp(ui, server)

