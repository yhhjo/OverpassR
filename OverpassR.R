library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(sp)
library(rgdal)
library(lubridate)
library(DT)
library(plyr)
library(dplyr)

library(xml2)
library(rvest)



#  ============================ GLOBAL VARIABLES =======================================

wrs <- st_read('Data/In/Landsat/wrs2_cleaned_datetime.shp') %>% filter(MODE == 'D')



# Lookup tables for Landsat 7 and 8 dates
ls8 <- read.csv('Data/In/Landsat/landsat8_lookup.csv') %>% cbind("Satellite" = "Landsat8")
ls7 <- read.csv('Data/In/Landsat/landsat7_lookup.csv') %>% cbind("Satellite" = "Landsat7")

#Used to select a table when toggling between satellites
lookup_table <- list("Landsat8" = ls8, "Landsat7" = ls7)
choices <- c("ls7", "ls8", "s2")

#Acquisition swath and mgrs tiles for Sentinel2
MGRS <- st_read('Data/In/Sentinel/Relevant_MGRS.shp')
SWATHS <- st_read('Data/In/Sentinel/Swaths.kml', layer = "NOMINAL")

global_table = data.frame()
global_coords = data.frame()

# Ensures landing page is displayed
LAUNCHING <-  TRUE


ui <- fluidPage(

  tags$head(tags$style(
    HTML(
      '.modal.in .modal-dialog{
        width:100%;
        height:100%;
        margin:0px;
      }

      .modal-content{
        width:100%;
        height:100%;
      }'

  
  fixedRow(
    column(4, offset = 5,
           titlePanel("OverpassR")) #,
  ),
  
  fixedRow(
    column(10, offset = 3, 
           mainPanel("Click on map or enter coordinates to view satellite overpass information")

    )
  )),
  titlePanel(tags$table(
    id = "title-panel",
    tags$tr(
      tags$td(
        style = "width: 25%",
        img(src = "Cuahsi_logo.png",
            style = "width: 100%; float: left")
      ),
      tags$td(style = "width: 13%"),
      tags$td(style = "width: 24%", "Overpasser"),
      tags$td(style = "width: 13%"),
      tags$td(
        style = "width: 25%",
        img(src = "UNC_logo_flat.png",
            style = "width: 80%; height 80%; float: right")
      )
    )
  )),
  
  
  tags$head(tags$style(
    HTML(
      " #inputs-table  {
          border-collapse: collapse;
        }
        #inputs-table td {
          padding: 5px;
          vertical-align: bottom;
        }"
    )
  )),
  
  #Upper menu with dates, coord search, and satellite dropdown
  wellPanel(
    class = "col-md-11.5",
    style = "margin: 20px 10px 20px 10px",
    tags$table(
      id = "inputs-table",
      style = "width: 100%",
      tags$tr(
        tags$td(
          style = "width: 10%",
          textInput(
            "lat",
            label = "Coordinates:",
            value = "",
            placeholder = "Lat"
          )
        ),
        tags$td(
          style = "width: 10%",
          textInput(
            "lon",
            label = NULL ,
            value = "",
            placeholder = "Lon"
          )
        ),
        tags$td(
          style = "width: 5%",
          div(class = "form-group shiny-input-container",
              actionButton("find", label = "Find"))
        ),
        tags$td(style = "width: 8%"),
        tags$td(
          style = "width: 33%; text-align: center",
          dateRangeInput(
            "dates",
            "Date range:",
            start = Sys.Date(),
            end = Sys.Date() + 16,
            min = "1999-05-15"
          ),
          span(textOutput('helpText'), style = "color:red")
        ),
        tags$td(style = "width: 8%"),
        tags$td(
          style = "width: 20%",
          selectInput(
            'timezone',
            "Timezone",
            choices = OlsonNames() %>% as.character(),
            selected = "GMT"
          )
        ),
        
        tags$td(style = "width: 8%"),
        tags$td(
          style = "width: 25%",
          checkboxGroupInput(
            "satellite",
            label = "Satellite:",
            choices = c(
              "Landsat7" = "ls7",
              "Landsat8" = "ls8",
              "Sentinel2" = "s2"
            ),
            selected = "ls7"
          )
        )
        
      )
    )
  ),
  
  # Map and help button
  wellPanel(
    tags$table(
      id = "leaflet-table",
      style = "width: 100%",
      actionButton(inputId = "help_button", "", icon = icon("question-circle")),
      tags$tr(
        tags$td(style = "width: 8%"),
        tags$td(style = "width: 84%",
                leafletOutput('map', height = 550)),
        tags$td(style = "width: 8%")
      )
    )
  ),
  
  # Download, refresh, and output table
  tags$table(tags$tr(
    tags$td(style = "width: 75%"),
    tags$td(style = "width: 5%",
            actionButton("refreshButton", " Reset ")),
    tags$td(style = "width: 10%",
            downloadButton('download', "Download"))
  ), ),
  
  DTOutput('table'),
  
)


#  ========================================== SERVER  ===============================================

server <- function(input, output, session) {
  proxy_table = dataTableProxy('table')
  proxyMap <- leafletProxy("map")
  
  
  # ====== Help dialogue / landing page ======
  observe({
    if (!is.null(input$help_button) || LAUNCHING) {
      LAUNCHING <<- FALSE
      relativ_date <-
        SWATHS$begin %>% as.Date() %>% max() %>% as.character()
      
      showModal(modalDialog(
        footer = modalButton("Go"),
        h1('Welcome to Overpasser! (beta)'),
        tags$p(
          tags$br(),
          tags$blockquote(
            "Overpasser was designed for integrating satellite remote sensing and field data collection. It is an interactive tool that
                           visualizes the location and footprint of satellite overpasses (or tiles, such as Landsat 7, 8, and Sentinel 2A/B) as well
                          as date/times. OverpassR can help researchers plan field campaigns during satellite overpasses as well as to simply visualize
                          the spatial and temporal coverage of satellite images over study areas."),
          tags$br(),
          tags$blockquote(
            tags$b("Directions:"),
            tags$ol(
              tags$li("Select your preferred satellites."),
              tags$li(
                "Click on the map (in as many locations as desired) or manually enter coordinates to see overpass locations on map and a table of dates.
              (The table can be interactively sorted by different columns by clicking the header)."),
              tags$li(
                "Click the “Download” button at the bottom to generate a .csv file of the table of overpass dates."),
              
              tags$li("Hit “Reset” to clear selections and start over.")
            )
          ),
          tags$i(
            tags$p(
              "Please send bug reports to the Lead Developer, Andrew Buchanan at the email address below.
                          Contact Simon Topp and John Gardner with feedback on current or desired future functionality."),
            p("Lead Developer: Andrew Buchanan (ajb28@live.unc.edu)."),
            p("Project Guidance: Simon Topp (sntopp@live.unc.edu), John Gardner (johngardner87@gmail.com), and Tamlin Pavelsky.")
          ),
          tags$b(
            tags$a(href = "http://uncglobalhydrology.org/", "Global Hydrology Lab")
          )
        ),
        tags$hr(),
        tags$p(
          tags$b("Resources:"),
          tags$ol(
            tags$li("Landsat: "),
            tags$i(
              tags$a(href = "https://github.com/yhhjo/OverpassR/blob/feature-ls-times/Data/In/Landsat/wrs2_cleaned_datetime.shp", "WRS shapefile & acquisition times"),
              br(),
              tags$a(href = "https://landsat.usgs.gov/landsat_acq", "Acquisiton dates")
            ),
            tags$li("Sentinel: "),
            tags$i(
              tags$a(href = "https://hls.gsfc.nasa.gov/wp-content/uploads/2016/03/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml", "MGRS tiles"),
              br(),
              tags$a(href = "https://github.com/yhhjo/OverpassR/blob/feature-ls-times/Data/In/Sentinel/Swaths.kml", 
                     paste0("Datetime acquisition swath .kml for ", 
                             SWATHS$begin %>% as.Date() %>% min() %>% as.character(),  " to ", 
                             SWATHS$begin %>% as.Date() %>% max() %>% as.character())
                     ),
            )
          )
        )
      ))
      get_swaths()
    }
  })
  
  
  # ====== Render map with WorldImagery and Labels as base ======
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = .75)) %>%
      setMaxBounds(
        lng1 = 180,
        lat1 = 90,
        lng2 = -180,
        lat2 = -90
      ) %>%
      addTiles(group = "Standard") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Relief") %>%
      addProviderTiles(providers$Esri.DeLorme, group = "Topographic") %>%
      setView(lat = 10,
              lng = 0,
              zoom = 2)  %>%
      addLayersControl(
        baseGroups = c("Satellite", "Standard", "Relief", "Topographic"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c('red', 'blue'),
        labels = c("Sentinel", "Landsat"),
        opacity = .5,
        title = "Tiles"
      )
    
  })
  
  
  # ====== Handle timezone changes ======
  observeEvent(input$timezone, {
    if (is_empty(global_coords)) {
      return()
    }
    
    for (i in 1:length(global_table$Time)) {
      if (is.na(global_table$Time[i])) {
        next
      } else {
        global_table$Time[i] <<-
          paste0(global_table$Date[i], global_table$Time[i]) %>% as.POSIXct() %>%
          strftime(format = "%H:%M:%S",
                   tz = input$timezone,
                   usetz = TRUE) %>% as.character.Date()
      }
    }
    display_global()
  })
  
  
  # ====== Manually entered coordinate search ======
  observeEvent(input$find, {
    lon <- input$lon %>% as.numeric()
    lat <- input$lat %>% as.numeric()
    
    if (!validCoords(lon, lat)) {
      output$helpText <-
        renderText("Enter valid coordinates. High tile density in poles limites latitude to (-82,82)")
      return()
    }
    
    global_coords <<-
      rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    
    # Calls generate and generateS2 based on which satellites are selected in input
    handleNewCoords(lon, lat)
    
    
  })
  
  
  # ======  Retroactive satellite changes ====== 
  observeEvent(input$satellite, {
    
    #Ignore if this is't retroactive
    if (is_empty(global_coords)) {
      return()
    } 
    global_table <<- data.frame()
    leafletProxy("map") %>% clearShapes()
    
    # Repopulate cleared data table and map
    for (r in 1:nrow(global_coords)) {
      handleNewCoords(global_coords$Long[r], global_coords$Lat[r])
    }
  })
  
  
  # ====== Handle user map clicks ======
  observeEvent(input$map_click, {
    if (is.null(input$map_click)) {
      return()
    }

    click <- input$map_click
    lon <- click$lng
    lat <- click$lat
    
    global_coords <<- rbind(global_coords, cbind("Long" = lon, "Lat" = lat)) %>% distinct()
    
    handleNewCoords(lon, lat)
  })
  
  
  # ====== Refresh button: use proxy map to clear tiles ======
  observeEvent(input$refreshButton, {
    
    proxyMap %>% clearShapes() %>% clearMarkers()
    output$helpText <- renderText({
      
    })
    updateDateRangeInput(session,
                         "dates",
                         "Date range:",
                         start = Sys.Date(),
                         end = Sys.Date() + 16)
    
    output$table <- renderDT(NULL)
    global_table <<- data.frame()
    global_coords <<- data.frame()
  })
  
  
  # ====== Retroactively update output table with a new date filter ======
  observeEvent(input$dates, {
    if (is.null(input$dates))
      return()
    
    if (!validDates() | is_empty(global_coords)) {
      return()
    } else {
      output$helpText <- renderText({})
    }
    
    #Clear output table and global table. Re-populate with global coords
    output$table <- renderDT(NULL)
    global_table <<- data.frame()
    leafletProxy("map") %>% clearShapes()
    
    for (r in 1:nrow(global_coords)) {
        handleNewCoords(global_coords$Long[r], global_coords$Lat[r])
    }
  })
  
  # ====== Observes download button ======
  output$download <- downloadHandler(
    filename = "overpassR.csv",
    content = function(file) {
      write.csv(global_table, file, row.names = TRUE)
    }
  )
  
  
  # ============ Server Helper Functions ============
  
  
  # ====== SENTINEL2 ======
  ## Given coordinates, display the footprints of MGRS tiles and update the output table
  generateS2 <- function(lon, lat) {
    start <- input$dates[1] %>% as.Date()
    stop <- input$dates[2] %>% as.Date()
    click <-
      cbind(lon, lat) %>% as.data.frame() %>% SpatialPoints() %>% st_as_sf(coords = c('lon', 'lat'))
    st_crs(click) = 4326
    
    #Swaths and mgrs that contain click
    swaths <- SWATHS[st_intersects(click, SWATHS, sparse = FALSE),]
    swaths$begin <- swaths$begin %>% ymd_hms()
    mgrs <- MGRS[st_intersects(click, MGRS, sparse = FALSE),]
    output_table <- data.frame()
    
    if (is_empty(swaths$begin) | is_empty(mgrs)) {
      map(lon, lat)
      output$helpText <-
        renderText(
          "Sentinel2 captures images over land. Some coordinates did not yield overpass information"
        )
      return(NULL)
    }
    
    #Update the map
    map(lon, lat) %>%  addPolygons(
      data = mgrs,
      color = 'red',
      weight = 2,
      label = paste0('ID: ', mgrs$Name),
      highlightOptions = highlightOptions(
        color = 'white',
        weight = 3,
        bringToFront = TRUE
      )
    )
    
    
    # Find the whole number of days between the swath's reference datetime and the start of the user's selected range (date).
    daterange_offset <- (difftime(swaths$begin, start) %>% as.integer() %% 5) %>% ddays()
    dates <-  start + daterange_offset
    datetimes <- data.frame()
    times <-
      strftime(
        swaths$begin,
        format = "%H:%M:%S",
        tz = input$timezone,
        usetz = TRUE
      ) %>% as.character.Date()
    
    
    #Positive date range, but too narrow
    if (any(dates > stop)) {
      output$helpText <-
        renderText("Date range too narrow for some overpasses. Try a range of at least 5 days.")
      return()
    }
    
    for (i in 1:length(dates)) {
      x <- seq.Date(dates[i], stop, 5)
      x <- data.frame("Date" = x[x <= stop]) %>% as.character.Date()
      datetimes <- rbind(datetimes, merge(x, times[i]))
    }
    
    names(datetimes) <-  c("Date", "Time")
    
    output <- datetimes %>% as.character.Date() %>% merge(mgrs$Name)
    names(output) <- c("Date", "Time" , "MGRS")
    output <-
      cbind(
        output,
        "Path" = NA,
        "Row" = NA,
        "Lat" = lat,
        "Lon" = lon,
        "Satellite" = "Sentinel2"
      ) %>% unique() %>% update_output()
  }
  
  
  # ====== LANDSAT 7 & 8 ======
  ## Given coordinates and either LS8 or LS7 lookup table, update map with overlapping WRS tiles and output table
  generate <- function(lon, lat, ref) {
    df <- returnPR(lon, lat)
    start <- input$dates[1] %>% as.numeric()
    stop <- input$dates[2] %>% as.numeric()
    
    #Update the map
    map(lon, lat) %>%  addPolygons(
      data = df$geometry,
      color = 'blue',
      weight = 2,
      label = paste0('Path: ', df$PATH, '; Row: ', df$ROW),
      highlightOptions = highlightOptions(
        color = 'white',
        weight = 3,
        bringToFront = TRUE
      )
    )
    
    #Create range of dates from start - end
    range <- start:stop %>% as.Date('1970-01-01') %>% as.character()
    
    # What cycle, 1-16, is the first day in the date range
    start_date_cycle <- start - (ref$Cycle_Start[1] %>% as.Date() %>% as.numeric()) %% 16 + 1
    
    #Lookup table where every date in range has corresponding cycle
    table <- cbind("Date" = range, "Cycle" = getCycles(1, 16, length(range), start_date_cycle - 1)) %>% as.data.frame()
    
    if (ref$Satellite[[1]] == "Landsat8") {
      times <- df$L8_Time
    } else {
      times <- df$L7_Time
    }
    
    updating_table <- data.frame()
    
    for (r in 1:length(df$PATH)) {
      cycle <- ref[ref$Path == df$PATH[r],]$Cycle
      dates <- table[table$Cycle == cycle,]$Date %>% as.character()
      
      if (is_empty(dates)) {
        next
      } else if (!is.na(times[r])) {
        time <- paste0(Sys.Date(), times[r]) %>% as.POSIXct() %>% 
                  strftime(format = "%H:%M:%S", tz = input$timezone, usetz = TRUE) %>% as.character.Date()
      } else {
        time <- NA
      }
      
      updating_table <-
        rbind(updating_table,
          cbind(
            "Date" = dates,
            "Time" = df$L7_Time[r],
            "Path" = df$PATH[r],
            "Row" = df$ROW[r],
            "MGRS" = NA,
            "Lat" = round(lat, 5),
            "Lon" = round(lon, 5),
            "Satellite" = (ref$Satellite[[1]] %>% as.character())
          )
        )
    }
    update_output(updating_table)
  }
  
  
  # ====== Calls appropriate generate functions with a given coordinate depending on satellite selection ======
  handleNewCoords <- function(lon, lat) {
    if (any(input$satellite == "s2")) {
      generateS2(lon, lat)
    }
    
    if (any(input$satellite == "ls7")) {
      generate(lon, lat, ls7)
    }
    
    if (any(input$satellite == "ls8")) {
      generate(lon, lat, ls8)
    }
  }
  
  
  # ====== Updates map with user's latest click ======
  map <- function(lon, lat) {
    return(
      leafletProxy("map") %>%
        addTiles(group = "Default") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$CartoDB.VoyagerOnlyLabels, group = "Satellite") %>%
        addProviderTiles(providers$Esri.WorldPhysical, group = "Relief") %>%
        addProviderTiles(providers$Esri.DeLorme, group = "Topographic") %>%
        addMarkers(
          lng = lon,
          lat = lat,
          label = paste0('Lat ', round(lat, 2), '; Lon: ', round(lon, 2))
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Standard", "Relief", "Topographic"),
          options = layersControlOptions(collapsed = TRUE)
        )
    )
  }
  
  
  # ====== Processes a data frame to be added to the output table ======
  update_output <- function(append) {
    if (is_empty(append)) {
      output$helpText <- renderText("No overpass in selected date range")
      return()
    } else {
      output$helpText <- renderText({})
    }
    
    #Renders distinct output
    if (!is_empty(global_table)) {
      global_table <<- rbind(append, global_table) %>% as.data.frame()
      global_table <<-
        global_table[!duplicated(global_table[, 1:4]),]
    } else {
      global_table <<- append %>% as.data.frame()
    }
    display_global()
  }
  
  
  # ====== Displays the global output table ======
  display_global <- function() {
    output$table <<- renderDT(datatable(
      global_table,
      rownames = NULL,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        orderClasses = TRUE,
        order = list(0, 'asc')
      )
    ) %>%
      formatRound(c(6:7), 2))
  }
  
  
  # ====== Returns whether dates are a valid range. Handles error message ======
  validDates <- function() {
    if (input$dates[1] >= input$dates[2]) {
      output$helpText <- renderText("Please select a positive date range")
      return(FALSE)
    } else {
      output$helpText <- renderText({
        
      })
      return(TRUE)
    }
  }
  
  
  # ====== WEB SCRAPER ======
  # This function goes to ESA's website containing Sentinel Acquisition Plans, downloads the kml, and replaces the old one locally
  # Required conditions: existing local acquisition plan's latest referenced date in $begin attribute is at least 11 prior to today
  # Error handling: If error or warning is raised, the local plan will not be replaced and an error message is displayed, prompting
  # user to report bug to ajb28@me.com.
  
  get_swaths <- function() {
    # Fetch if today is more than 10 days over the latest acquisition SWATH
    if (!(SWATHS$begin %>% as.Date() %>% max() - today()) < -10) {
      return()
    } else {
      out <- tryCatch({
        url <-
          'https://sentinel.esa.int/web/sentinel/missions/sentinel-2/acquisition-plans'
        
        # Pull HTML and CSS source code
        page <- read_html(url)
        
        # Pulls specific attribute within webpage that contains the latest sentinel-2a acquisition swath URL extension
        link <-
          (page %>% html_nodes(".sentinel-2a") %>% html_nodes("a"))[1] %>% as.character()
        
        # Regex: match everything between "/d" and .kml on the hyperlink attribute
        regex <- "/d(.*)\\.kml"
        
        # Extracts the extension
        extension <- regmatches(link, regexpr(regex, link))
        url <- paste0("https://sentinel.esa.int", extension)
        
        # Download kml as temp file
        download.file(url, "temp.kml")
        stopifnot(file.rename("temp.kml", "./Data/In/Sentinel/Swaths.kml"))
        
        # Update local data
        SWATHS <<-
          st_read("Data/In/Sentinel/Swaths.kml", layer = "NOMINAL")
      },
      
      # === WARNING HANDLING ===
      warning = function(cond) {
        output$helpText <-
          renderText(
            "A warning flag was raised while updating ESA Sentinel 2 Database. Please report this bug to ajb28@live.unc.edu "
          )
      },
      
      # === ERROR HANDLING  ===
      error = function(cond) {
        output$helpText <-
          renderText(
            "An error occurred connecting to ESA Sentinel 2 Database. Please report this bug to ajb28@live.unc.edu "
          )
      })
    }
  }
}


# ============ Generic helpers ============

# ====== Takes lon, lat, shapefile, and returns PR of intersected tiles ======
returnPR <- function(lon, lat) {
  coords <- as.data.frame(cbind(lon, lat))
  point <- SpatialPoints(coords)
  proj4string(point) <-
    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  #Convert the point to a shape file so they can intersect
  pnt <-  st_as_sf(point, coords = c('lon' , 'lat'))
  
  bool_selector <- st_intersects(wrs, pnt, sparse = FALSE)
  return(wrs[bool_selector,])
  
}

# ====== Validates coordinate clicks ======
validCoords <- function(lon, lat) {
  if (is.null(lon) | is.null(lat)
      | is.na(lon) | is.na(lat))
    return(FALSE)
  
  return((lat > -82 && lat < 82) &&
           (lon >= -180 && lon <= 180))
  
}

# ====== Genrates sequence of Landsat overpass cycles ======
getCycles <- function(from, to, len, offset = 0) {
  cycles <- rep(1:16, length.out = (len + offset))
  return(cycles[(1 + offset):length(cycles)])
}

shinyApp(ui, server)
