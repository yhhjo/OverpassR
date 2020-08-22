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
wrs <- st_read('Data/In/Landsat/wrs2_asc_desc.shp')
wrs <- wrs[wrs$MODE == 'D',]

#Lookup tables for Landsat 7 and 8
ls8 <- read.csv('Data/In/Landsat/landsat8_lookup.csv') %>% cbind("Satellite" = "Landsat8")
ls7 <- read.csv('Data/In/Landsat/landsat7_lookup.csv') %>% cbind("Satellite" = "Landsat7")

#Used to select a table when toggling between satellites
lookup_table <- list("Landsat8"=ls8,"Landsat7"=ls7)
choices <- c("ls7","ls8","s2")

#Acquisition swath and mgrs tiles for Sentinel2
MGRS <- st_read('Data/In/Sentinel/Relevant_MGRS.shp')
SWATHS <- st_read('Data/In/Sentinel/Sentinel-2A_MP_ACQ_KML_20200716T120000_20200803T150000.kml', layer = "NOMINAL")
SWATHS$Overpass <- as.Date(SWATHS$begin, format = "%y/%m/%d")

#Global variables. It will update with each map click and reset only when 'reset map' button is clicked
global_table = data.frame()
global_coords = data.frame()


ui <- fluidPage( 
  
  tags$head(tags$style(HTML('
      .modal.in .modal-dialog{
        width:100%;
        height:100%;
        margin:0px;
      }

      .modal-content{
        width:100%;
        height:100%;
      }
    '))),
  
  
  fixedRow(
    column(4, offset = 5, titlePanel("OverpassR"))
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
                                 checkboxGroupInput("satellite",label = "Satellite:",
                                                    choices = c("Landsat7" = "ls7", "Landsat8" = "ls8", "Sentinel2" = "s2"),
                                                    selected = "ls7")
                         )
                       ) 
            ) 
  ), 
  
  wellPanel(
    tags$table(id = "leaflet-table", style = "width: 100%",
               tags$tr(
                 tags$td(style = "width: 8%"),
                 tags$td(style = "width: 84%",
                         leafletOutput('map',height = 550)
                 ),
                 tags$td(style = "width: 8%")
               )
    )
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
  
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = proxyMap, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      h1('Welcome to OverpassR!'),
      p("This is a beta version of a website aimed to help researchers incorporate remote sensing into their work. Select your preferred satellites and click
                     and click on the map or manually enter study site coordinates to see satellite overpass information. The table can be downloaded as a csv
                     by clicking the 'Download' button at the bottom. Click the 'Reset' button to clear all user input and start over." )
    ))
  })
  
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
    
    
    if(any(input$satellite == "s2"))
      generateS2(lon, lat)
    
    if(any(input$satellite == "ls7"))
      generate(lon, lat, ls7)
    
    if(any(input$satellite == "ls8"))
      generate(lon, lat, ls8)
    
  })
  
  #For switching between satellites once clicks have been made
  observeEvent(input$satellite,{
    
    #Ignore if this is NOT a retroactive click
    if(is_empty(global_coords))
      return()
    
    ##Clear DT and global table to be re-populated
    global_table <<- data.frame()
    
    leafletProxy("map") %>% clearShapes()
    
    #More efficient way of coding the following?
    if(any(input$satellite == "ls7")){
      for(row in 1:nrow(global_coords)){
        generate(global_coords$Long[row], global_coords$Lat[row], ls7)
      }
    }
    
    if(any(input$satellite == "ls8")){
      for(row in 1:nrow(global_coords)){
        generate(global_coords$Long[row], global_coords$Lat[row], ls8)
      }
    }
    
    
    if(any(input$satellite == "s2")){
      for (row in 1:nrow(global_coords)){
        generateS2(global_coords$Long[row], global_coords$Lat[row])
      }
    }
    
  })
  
  
  observeEvent(input$map_click,{
    
    if(is.null(input$map_click))
      return() 
    
    click <- input$map_click
    lon <- click$lng
    lat <- click$lat
    
    global_coords <<- rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    
    if(any(input$satellite == "s2"))
      generateS2(lon, lat)
    
    if(any(input$satellite == "ls7"))
      generate(lon, lat, ls7)
    
    if(any(input$satellite == "ls8"))
      generate(lon, lat, ls8)
    
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
      
      if(any(input$satellite == "s2"))
        generateS2(global_coords$Long[row], global_coords$Lat[row])
      if(any(input$satellite == "ls7"))
        generate(global_coords$Long[row], global_coords$Lat[row], ls7)
      if(any(input$satellite == "ls8"))
        generate(global_coords$Long[row], global_coords$Lat[row], ls8)
    }
    
  })
  
  #SENTINEL2: Updates map, global table, clobal coords given click with MGRS and swath data
  generateS2 <- function(lon, lat){
    
    start <- input$dates[1] %>% as.Date() %>% as.numeric()
    stop <- input$dates[2] %>% as.Date() %>% as.numeric()
    click <- cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon','lat'))
    st_crs(click) = 4326
    
    #Swaths and mgrs that contain click
    swaths <- SWATHS[st_intersects(click, SWATHS, sparse = FALSE),]
    mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]
    output_table <- data.frame()
    
    
    #Update the map
    leafletProxy("map") %>%
      addTiles(group = "Default") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
      setView(lng = lon , lat = lat, zoom = 6) %>%
      addPolygons(
        data = mgrs, color = 'red', weight = 2, label = paste0('ID: ', mgrs$Name),
        highlightOptions = highlightOptions(color = 'white', weight = 3, bringToFront = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Satellite", "Default"),
        options = layersControlOptions(collapsed = TRUE))
    
    #Swaths only cover land
    if(is_empty(swaths$Overpass)){
      output$helpText <- renderText("Sentinel2 captures images over land. Some coordinates did not yield overpass information")
      return(NULL)
    }
    
    within_range <- c(start + ((swaths$Overpass %>% as.Date() %>% as.numeric()  - start) %% 5) %>% unique())
    
    #Positive date range, but too narrow
    if(any(within_range>stop)){
      output$helpText <- renderText("Date range too narrow for some overpasses Try a range of at least 5 days.")
      return(NULL)
    }
    
    all_dates <- c(sapply(within_range, function(x) {seq(x, stop, by = 5) })) %>% unlist() %>% as.Date('1970-01-01') %>% as.character()
    
    df <- merge(all_dates, mgrs$Name)
    names(df) <- c("Dates", "MGRS")
    output_table <- cbind(df, "Path" = NA, "Row" = NA, "Lat" = lat, "Lon" = lon, "Satellite" = "Sentinel2")
    
    if(is_empty(output_table))
      return()
    
    output_table <- output_table %>% distinct()
    update_output(output_table)
    
  }
  
  
  
  #LANDSAT7&8: Updates map with tiles and global table with data given coordinates of a click
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
    CYCLE_1_REFERENCE <- ref$Cycle_Start[1] %>% as.Date() %>% as.numeric()
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
      
      updating_table <- rbind(updating_table, cbind("Dates" = dates, "Path" = paths[r], "Row" = rows[r], "MGRS" = NA,
                                                    "Lat" = round(lat,5), "Lon" = round(lon,5), "Satellite" = (ref$Satellite[[1]] %>% as.character()) ))
    }
    update_output(updating_table)
  }
  
  #Updates table 
  update_output <- function(updating_table){
    
    
    if(is_empty(updating_table)){
      output$helpText <- renderText("No overpass in selected date range")
      return(NULL)
    }
    
    else
      output$helpText <- renderText({})
    
    #Renders distinct output
    if(!is_empty(global_table)){
      global_table <<- rbind(updating_table, global_table) %>% as.data.frame()
      global_table <<- global_table[!duplicated(global_table[,1:4]),]
    }
    
    else
      global_table <<- updating_table %>% as.data.frame()
    
    #Display table
    output$table <<- renderDT( datatable(global_table, 
                                         rownames = NULL, options = list(paging = FALSE, searching = FALSE, info = FALSE, 
                                                                         orderClasses = TRUE, order = list(0, 'asc'))) %>%
                                 formatRound(c(5:6),2) )
    
    
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
