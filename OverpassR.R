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
library(plyr)
library(dplyr)

#Read the wrs tiles 
wrs <- st_read('Data/In/wrs2_asc_desc.shp')
wrs <- wrs[wrs$MODE == 'D',]

#Lookup tables for Landsat 7 and 8
ls8 <- read.csv('Data/In/Landsat/landsat8_lookup.csv') %>% cbind("Satellite" = "Landsat8")
ls7 <- read.csv('Data/In/Landsat/landsat7_lookup.csv') %>% cbind("Satellite" = "Landsat7")

#Used to select a table when toggling between satellites
lookup_table <- list("Landsat8"=ls8,"Landsat7"=ls7)


#Global variables. It will update with each map click and reset only when 'reset map' button is clicked
global_table = data.frame()
global_coords = data.frame()


ui <- fluidPage( 
  fixedRow(
    column(4, offset = 5, titlePanel("OverpassR"))
  ),
  
  fixedRow(
    column(12, offset = 2,
           mainPanel(
             "This website facilitates the planning of field work that wants to incorporate remote sensing into their study design.
                     Select your preferred satellite and click on the map or manually enter study site coordinates to see when the next satellite
                     overpass will occur for the selected area. For extended periods, enter a date range to extract all overpasses within the given time period.
                     The date information can be downloaded as a csv by clicking the 'Download' button at the bottom." 
           )
    )
  ), 
  
  tags$head(
    tags$style(
      HTML(
        " #inputs-table  {
          border-collapse: collapse;
        }
        #inputs-table td {
          padding: 5px;
          vertical-align: bottom; 
        }"
      ) 
    ) 
  ),
  
  #Upper menu with dates, coord search, and satellite dropdown
  wellPanel(class = "col-md-11.5", style = "margin: 20px 10px 20px 10px",
            tags$table(id = "inputs-table",style = "width: 100%",
                       tags$tr(
                         tags$td(style = "width: 10%",
                                 textInput("lat",label = "Coordinates:", value = "", placeholder = "Lat")
                         ),
                         tags$td(style = "width: 10%",
                                 textInput("lon", label = NULL ,value = "", placeholder = "Lon")
                         ),
                         tags$td(
                           style = "width: 5%",
                           div(class = "form-group shiny-input-container",
                               actionButton("find", label = "Find"))
                         ),
                         tags$td(style = "width: 8%"),
                         tags$td( style = "width: 33%; text-align: center",
                                  dateRangeInput("dates", "Date range:", start = Sys.Date(), end = Sys.Date() + 16),
                                  span(textOutput('helpText'), style = "color:red")
                         ),
                         
                         tags$td(style = "width: 12%"),
                         tags$td(style = "width: 25%",
                                 selectInput("satellite",label = "Satellite:",choices = c("Landsat7", "Landsat8"))
                         )
                       ) 
            ) 
  ), 
  
  wellPanel(
    leafletOutput('map', width = '100%', height = 550),
  ),
  
  tags$table(
    tags$tr(
      tags$td(style = "width: 75%"),
      tags$td(style = "width: 5%",
              actionButton("refreshButton", " Reset ")),
      tags$td(style = "width: 10%",
              downloadButton('download', "Download"))
    ),
  ),
  
  DTOutput('table')
  
)#End of ui



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
      output$helpText <- renderText("Enter valid coordinates. High tile density in poles limites latitude to (-82,82)")
      return()
    }
    
    global_coords <<- rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    generate(lon, lat, lookup_table[[input$satellite]])
    
  })
  
  #For switching between satellites once clicks have been made
  observeEvent(input$satellite,{
    
    #Ignore if this is NOT a retroactive click
    if(is_empty(global_coords))
      return()
    
    #Clear DT and global table to be re-populated
    output$table <- renderDT(NULL)
    global_table <<- data.frame()
    
    leafletProxy("map") %>% clearShapes()
    for(row in 1:nrow(global_coords)){
      generate(global_coords$Long[row], global_coords$Lat[row], lookup_table[[input$satellite]])
    }
    
  })
  
  
  observeEvent(input$map_click,{
    
    if(is.null(input$map_click))
      return() 
    
    click <- input$map_click
    lon <- click$lng
    lat <- click$lat
    
    global_coords <<- rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    
    generate(lon, lat, lookup_table[[input$satellite]])
    
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
    
    
  })
  
  #Retroactively update output table with a new date filter
  observeEvent(input$dates,{
    
    if(is.null(input$dates))
      return()
    
    #Check valid dates
    if(input$dates[1] >= input$dates[2]){
      output$helpText <- renderText("Please select a positive date range")
      return()
    }
    
    else{
      output$helpText <- renderText({})
    }
    #Ignore if this is NOT a retroactive click
    if(is_empty(global_coords))
      return()
    
    #Clear DT and global table to be re-populated
    output$table <- renderDT(NULL)
    global_table <<- data.frame()
    leafletProxy("map") %>% clearShapes()
    
    
    for(row in 1:nrow(global_coords)){
      generate(global_coords$Long[row], global_coords$Lat[row], lookup_table[[input$satellite]])
    }
    
  })
  
  
  #Updates map with tiles and global table with data given coordinates of a click
  generate <- function(lon, lat, ref){
    
    ##Here, WRS variable can be replaced with the list of checked boxes
    df <- returnPR(lon, lat, wrs)
    paths <- df$path
    rows <- df$row
    tile_shapes <- df$shape.geometry
    
    #Update the map
    leafletProxy("map") %>%
      addTiles(group = "Default") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
      setView(lng = lon , lat = lat, zoom = 6) %>%
      addPolygons(
        data = tile_shapes, color = 'blue', weight = 2, label = paste0('Path: ',paths,'; Row: ', rows),
        highlightOptions = highlightOptions(color = 'white', weight = 3, bringToFront = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Default"),
        options = layersControlOptions(collapsed = TRUE))
    
    start_date <- input$dates[1] %>% as.numeric()
    end_date <- input$dates[2] %>% as.numeric()
    
    #Be sure user selects a positive date range
    if(start_date >= end_date){
      output$helpText <- renderText("Please select a positive date range")
      return(NULL)
    }
    
    #Create range of dates from start - end
    range <- start_date:end_date %>% as.Date('1970-01-01') %>% as.character()
    
    #Find a known start date for the cycle
    CYCLE_1_REFERENCE <- ref$Cycle_Start[1] %>% as.Date() %>% as.numeric()
    
    #Find 'start date's' cycle 
    start_date_cycle <- (start_date - CYCLE_1_REFERENCE) %% 16 + 1
    
    #Lookup table where every date in rnge has corresponding cycle
    table <- cbind("Date" = range, "Cycle" = getCycles(1, 16, length(range), 
                                                       start_date_cycle - 1 )) %>% as.data.frame()
    
    updating_table <- NULL
    
    for(r in 1:length(paths)){
      cycle <- ref[ref$Path==paths[r],]$Cycle
      dates <- table[table$Cycle==cycle,]$Date %>% as.character()
      
      if(is_empty(dates))
        next
      
      updating_table <- rbind(updating_table, cbind("Dates" = dates, "Path" = paths[r], "Row" = rows[r], "Lat" = round(lat,5), "Long" = round(lon,5), "Satellite" = input$satellite ))
    }
    
    
    if(is_empty(updating_table)){
      output$helpText <- renderText("No overpass in selected date range")
      return(NULL)
    }
    
    else
      output$helpText <- renderText({})
    
    
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
    (lat>-82 && lat<82) && 
      (lon>=-180 && lon<=180)
  )
  
} 



getCycles <- function(from, to, len, offset = 0){
  cycles <- rep(1:16, length.out = (len+offset))
  return(cycles[(1+offset):length(cycles)])
}



shinyApp(ui, server)
