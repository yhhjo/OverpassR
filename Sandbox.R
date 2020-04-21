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
lookup_table <- read.csv('data/in/landsat8_lookup.csv')

#Global variables. It will update with each map click and reset only when 'reset map' button is clicked
global_table = data.frame()
global_coords = data.frame()


ui <- fluidPage(
  
  fixedRow(
    column(4, offset = 5,
           titlePanel("OverpassR"))
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
    column(4, offset = 4, 
           textOutput('helpText')),
    column(1, offset = 8,
           actionButton('applyDates', "Apply Date Filter")
    )
  ),
  
  fluidRow(
    DTOutput('table')
  ),
  
  fixedRow(
    mainPanel(" ")
  ),
  
  fixedRow(
    column(1, offset = 8, 
           downloadButton('download', "Download") )
  )
)



server <- function(input, output, session){
  
  proxy_table = dataTableProxy('table')
  proxyMap <- leafletProxy("map")
  
  
  #Render map with base layers WorldImagery and Labels
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = .75)) %>%
      setMaxBounds(lng1 = 180, lat1 = 90, lng2 = -180, lat2 = -90)%>%
      addTiles(group = "Default") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
      setView(lat=10, lng=0, zoom=2)  %>%
      addLayersControl(
        baseGroups = c("Satellite", "Default"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  observeEvent(input$find,{
    
    lon <- input$lon %>% as.numeric()
    lat <- input$lat %>% as.numeric()
    
    if(!validCoords(lon, lat)){
      output$helpText <- renderText("Enter valid coordinates")
      return()
    }
    
    global_coords <<- rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    
    generate(lon, lat)
  })
  
  
  observeEvent(input$map_click,{
    
    if(is.null(input$map_click))
      return() 

    click <- input$map_click
    lon <- click$lng
    lat <- click$lat
    
    global_coords <<- rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    
    generate(lon, lat)
    
  })
  
  
  #Use a separate observer to clear shapes and output table if "clear tiles" button clicked
  observeEvent(input$refreshButton,{
    
    if(is.null(input$refreshButton))
      return()
    
    proxyMap%>% clearShapes()
    output$helpText <- renderText({})
    updateDateRangeInput(session, "dates", "Date range:",
                         start = Sys.Date(), end = Sys.Date()+16)
    
    output$table <- renderDT(NULL)
    global_table <<- data.frame()
    global_coords <<- data.frame()
    
    
  }
  )
  
  #Retroactively update output table with a new date filter
  observeEvent(input$applyDates,{

    #Check valid dates
    if(input$dates[1] >= input$dates[2]){
      output$helpText <- renderText("Please select a positive date range")
      return()
    }
    
    #Ignore if this is NOT a retroactive click
    if(is_empty(global_coords))
      return()
    
    #Clear DT and global table to be re-populated
    output$table <- renderDT(NULL)
    global_table <<- data.frame()
    
    for(row in 1:nrow(global_coords)){
      generate(global_coords$Long[row], global_coords$Lat[row])
    }
    
  })
  
  #Updates map with tiles and global table with data given coordinates of a click
  generate <- function(lon, lat){
    
    ##Here, WRS variable can be replaced with the list of checked boxes
    df <- returnPR(lon, lat, wrs)
    paths <- df$path
    rows <- df$row
    tile_shapes <- df$shape.geometry
    
    #Shape file of tiles that intersect
    reference_date <- lookup_table[paths,]$Overpass
    
    #Handles if date filter is applied  
    
    start_date <- input$dates[1]
    end_date <- input$dates[2]
    
    #Be sure user selects a positive date range
    if(start_date >= end_date){
      output$helpText <- renderText("Please select a positive date range")
      return(NULL)
    }
    
    days_til <- (start_date %>% as.numeric() - reference_date ) %% 16
    next_pass <- start_date + days_til
    
    #No overpass in the selected range
    #Try (end_date < next_pass)
    if( all(next_pass > end_date) ){
      output$helpText <- renderText("No Overpasses in selected date range")
      return(NULL)
    }
    
    #Clear text output if a valid date range is entered
    else {
      output$helpText <- renderText({})
    }
    
    len <- 1:length(next_pass)
    updating_table <- NULL
    
    #Each loop iteration creates a data frame "temp" of dates, path, row, lat, lon for one tile
    ## Subseqeuent iterations create a temp data frame, then append the new data frame to the previous one
    for (r in len){
      if(next_pass[r] > end_date)
        next
      dates <- seq.Date(next_pass[r], to = end_date, by = 16) %>% as.character()
      temp <- cbind("Date" = dates, "Path" = paths, "Row" = rows, "Lat" = round(lat,5), "Long" = round(lon,5))
      updating_table <- rbind(updating_table, temp)
    }
    
    
  
    #Renders distinct output
    if(!is_empty(global_table)){
      global_table <<- rbind(updating_table, global_table) %>% as.data.frame()
      x <- duplicated(global_table[,1:3])
      global_table <<- global_table[!x,]
    }
    
    else
      global_table <<- updating_table %>% as.data.frame()
    
    #Display table
    output$table <<- renderDT(
      global_table, rownames = NULL, options = list(paging = FALSE, searching = FALSE, info = FALSE) 
    )
    
    #Update the map: clear all tiles, then add only the ones that overlap in Red
    leafletProxy("map") %>%
      addTiles(group = "Default") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
      setView(lng = lon , lat = lat, zoom = 6) %>%
      addPolygons(
        data = tile_shapes, color = 'blue', weight = 2, 
        highlightOptions = highlightOptions(color = 'white', weight = 3, bringToFront = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Default"),
        options = layersControlOptions(collapsed = TRUE))
  }
  
  output$download <- downloadHandler(
    filename = "overpassR.csv",
    content = function(file){
      write.csv(global_table, file, row.names = TRUE)
    }
  )
}






#Helper methods---------------------------------------------------------------------------------------------------


#Helper method takes lon, lat, shapefile, and returns PR of intersected tiles 
returnPR <- function(lon, lat, shapefile){
  
  #Validate input.
  #PROBLEM: Why is shiny not rendering this output message? 
  validate(
    need(validCoords(lon, lat), "Enter valid coordinates")
  )
  
  coords <- as.data.frame(cbind(lon, lat)) 
  point <- SpatialPoints(coords)
  proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  #Convert the point to a shape file so they can intersect
  pnt <-  st_as_sf(point, coords = c('lon' , 'lat'))
  
  #Create a boolean matrix of all the polygons that intersect pnt (a user's mapclick)
  ## And select those polygons from WRS
  bool_selector <- st_intersects(shapefile, pnt, sparse = FALSE)
  tiles <- (shapefile[bool_selector,])
  paths <-  tiles$PATH
  rows <- tiles$ROW
  
  return(data.frame("path" = paths, "row" = rows, "shape" = tiles))
  
}

validCoords <- function(lon, lat){
  
  if(is.null(lon) | is.null(lat)
     | is.na(lon) | is.na(lat))
    return(FALSE)
  
  return(
    (lat>=-90 && lat<=90) && 
      (lon>=-180 && lon<=180)
  )
  
} 







shinyApp(ui, server)
