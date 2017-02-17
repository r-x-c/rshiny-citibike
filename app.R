library(shiny)
library(leaflet)
library(plyr)


citibikedata <- read.csv("./data/citibike.csv")
locationData <- citibikedata[,c("bikeid", "stoptime", "end.station.latitude", "end.station.longitude")]
locationData <- transform(locationData, locId=paste0(end.station.latitude,end.station.longitude))
names(locationData) <- c("bikeId", "stopTime", "latitude", "longitude", "locId")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                wellPanel(
                selectizeInput("bikeInput", label = "See where you bike has been!", choices = unique(locationData$bikeId),
                               options = list(maxOptions = 5, maxItems = 1, placeholder = 'enter bike id')),
                helpText(   a("view on github", href="https://github.com/r-x-c/rshiny-citibike", target="_blank")
                )
                )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'bikeInput', choices = unique(locationData$bikeId), server = TRUE)
  
  
  # Reactive expression for the data subsetted to what the user selected
  bikeHistorySubset <- reactive({
    if (input$bikeInput == "") {
      ddply(locationData[locationData$bikeId == 18466,], .(locId), transform, freq = length(locId));
    }
    else {
      ddply(locationData[locationData$bikeId == input$bikeInput,], .(locId), transform, freq = length(locId));
    }
  })
  
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
        attribution = "Weather data © 2012 IEM Nexrad"
      ) %>%
      fitBounds(~min(locationData$longitude), ~min(locationData$latitude),
                ~max(locationData$longitude), ~max(locationData$latitude))
     
    
  })
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = bikeHistorySubset()) %>%
      clearShapes() %>%
      addCircles(radius = ~freq * 40, weight = ~sqrt(freq), fillOpacity = .6, popup = ~paste(freq)
      )
  })
  
}

shinyApp(ui, server)
