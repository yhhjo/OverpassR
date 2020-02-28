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
  sample_frac(.05)

lookup_table <- read.delim('data/in/lookup_table.txt')

pal <- colorNumeric(palette = "Blues", domain = wrs$PATH )

test <- leaflet(wrs) %>% 
  addTiles() %>% 
  addPolygons(stroke = TRUE, color = ~pal(PATH), layerId = ~PR )




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
  
  output$table <- renderTable(lookup_table)
  
  
  
  }




shinyApp(ui, server)

