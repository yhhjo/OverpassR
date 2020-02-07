library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(sp)
library(rgdal)

  #Read the wrs tiles 
  wrs <- 
    st_read('data/in/wrs2_asc_desc.shp')%>%
    sample_frac(.05)
  
  
  pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )
  
  test <- leaflet(wrs) %>% 
    addTiles() %>% 
    addPolygons(stroke = TRUE, color = ~pal(PATH), layerId = ~PR )
  
  
  
  
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
    



shinyApp(ui, server)

