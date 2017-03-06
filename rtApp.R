library(shiny)
library(leaflet)
library(plyr)
library(jsonlite)
library(RCurl)

# Richard Chen, rxc@umich.edu

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                h2("CitiBike Hotspots"),
                helpText(   a("@github", href="https://github.com/r-x-c/rshiny-citibike", target="_blank"))
  )
)

server <- function(input, output, session) {
  refresh_table <- function() {
    # 1. PREPARE DATA
    # Import and flatten station_status JSON into r dataframe
    station_JSON <- fromJSON(getURL("https://gbfs.citibikenyc.com/gbfs/en/station_status.json"), flatten = TRUE)
    station_data <- station_JSON$data$stations
    id_JSON <- fromJSON(getURL("https://gbfs.citibikenyc.com/gbfs/en/station_information.json"), flatten = TRUE)
    id_data <- id_JSON$data$stations
    # Relevant stations are: is_installed, is_renting, and is_returning
    active_stations <- subset(station_data, is_installed == 1 & is_renting == 1 & is_returning == 1)
    # Select desired data
    id_data <- subset(id_data, select = c("station_id","capacity", "name", "lat", "lon"))
    active_stations <- subset(active_stations, select = c("station_id","num_bikes_available", "num_bikes_disabled"))
    # Cast to int, then merge
    active_stations$station_id <- as.integer(active_stations$station_id)
    id_data$station_id <- as.integer(id_data$station_id)
    active_stations <- merge(active_stations, id_data, by = "station_id")
    # 2. Computations to analyze data
    idleMedian <- median(active_stations$num_bikes_available)
    active_stations$idleDelta = active_stations$num_bikes_available - idleMedian
    active_stations$type = (active_stations$idleDelta > 0)
    # more negative means station is a more popular hotspot
    return(subset(active_stations, select = c("name","lat", "lon", "idleDelta", "type")))
  }
  
  output$map <- renderLeaflet({
    # aspects of map that won't have to change dynamically
    # CartoDB.Positron: simpler grey scale map to emphasize data pts
    # WMSTtiles for weather conditions
    leaflet(quakes) %>% addProviderTiles("CartoDB.Positron") %>%
      addTiles(options = providerTileOptions(opacity = 0.4)) %>%  # Add default OpenStreetMap map tiles
      addWMSTiles(
        "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
        layers = "nexrad-n0r-900913",
        options = providerTileOptions(opacity = 0.35),
        attribution = "Weather data ?? 2012 IEM Nexrad"
      )
  })
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  pal <- colorFactor(c("#e80404", "#04e008"), domain = c(TRUE, FALSE)) # 1 popular 0 arid
  observe({
    # update every 5 seconds
    invalidateLater(5000, session)
    leafletProxy("map", data = refresh_table()) %>%
      clearShapes() %>%
      addCircles(radius = ~abs(idleDelta) * 5, 
                 fillOpacity = .6, 
                 popup = ~paste(name),
                 color = ~pal(type)
      ) %>%
      fitBounds(~min(lon), ~min(lat),
                ~max(lon), ~max(lat))
  })
  
}

shinyApp(ui, server)
