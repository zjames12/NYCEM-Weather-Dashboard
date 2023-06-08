library(shinydashboard)
library(leaflet)
source("noaa_api.R")


function(input, output, session) {
  getData <- reactive({
    input$refresh # Refresh if button clicked
    
    # Get interval (minimum 30)
    interval <- max(as.numeric(input$interval), 30)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)
    call_api_2()
  })
  getNYCMap <- reactive(
    nyc_outline()
  )
  output$mapPlot <- renderLeaflet({
    # nyc <- nyc_outline()
    # layer <- call_api_2()
    # if (input$layer == "Temperature"){
    #   plot_temp_map(getNYCMap(), getData())
    # } else if (input$layer == "Relative Humidity") {
    #   plot_rh_map(getNYCMap(), getData())
    # }
    leaflet_plot(getData(), getNYCMap(), input$layer)
    
    
    
  })
  
  lastUpdateTime <- reactive({
    getData() # Trigger this reactive when vehicles locations are updated
    Sys.time()
  })
  
  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 5 seconds
    invalidateLater(5000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })
  
}