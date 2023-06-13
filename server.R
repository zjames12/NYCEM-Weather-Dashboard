library(shiny)
library(shinydashboard)
library(leaflet)
library(formattable)

source("noaa_api.R")


function(input, output, session) {
  getData <- reactive({
    input$refresh # Refresh if button clicked
    
    # Get interval (minimum 30)
    # interval <- max(as.numeric(input$interval), 30)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    # invalidateLater(interval * 1000, session)
    call_api_2()
  })
  getNYCMap <- reactive(
    nyc_outline()
  )
  
  getCentralParkData <- reactive(
    api_call_central_park()
  )
  output$mapPlot <- renderLeaflet({
    # api_data_plot(getData(), getNYCMap(), input$layer)
    # field = input$layer
    layer_data = getData()
    
    
    m <- leaflet() %>%
      addMapPane("background_map", zIndex = 410) %>%  
      addMapPane("polygons", zIndex = 420) %>%        
      addMapPane("polylines", zIndex = 430) %>%
      addMapPane("labels", zIndex = 440) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       layerId = "background_map",
                       # options = providerTileOptions(minZoom = 9, maxZoom = 12),
                       options = pathOptions(pane = "background_map")
                       ) %>%
      fitBounds(-74.302575, 40.496949, -73.738228, 40.883178) %>%
      
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, 
                       options = pathOptions(pane = "labels"))
      # leaflet::addLegend("bottomright",pal = pal, values = legend_values,
      #                    opacity = 1, title = legend_title)
    m
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  observe({
    field = input$layer
    layer_data = getData()
    
    if (field == "Temperature"){
      data_to_plot = c_to_f(layer_data$temp)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Spectral", domain = data_to_plot, reverse = F)
      legend_title = "&degF"
      legend_values = data_to_plot
    } 
    else if (field == "Relative Humidity") {
      data_to_plot = layer_data$rh
      labels <- sprintf(
        "<strong>%.0f&#37</strong>",
        layer_data$rh
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Greens", domain = data_to_plot)
      legend_title <- "&#37"
      legend_values = data_to_plot
    } 
    else if (field == "Max. Temperature") {
      data_to_plot = c_to_f(layer_data$maxTemp)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Spectral", domain = data_to_plot, reverse = T)
      legend_title = "&degF"
      legend_values = data_to_plot
    } 
    else if (field == "Min. Temperature") {
      data_to_plot = c_to_f(layer_data$minTemp)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = "Spectral", domain = data_to_plot, reverse = T)
      legend_title = "&degF"
      legend_values = data_to_plot
    } 
    else {
      data_to_plot = layer_data$rh
      labels <- sprintf(
        "<strong>%s, %s</strong>",
        layer_data$X, layer_data$Y
      ) %>% lapply(htmltools::HTML)
      
      bins <- 0:10 * 10
      pal <- colorBin("Spectral", domain = layer_data$temp, bins = bins)
      legend_title <- "&#37"
      legend_values = data_to_plot
    }
    leafletProxy("mapPlot", data = layer_data) %>%
      clearShapes() %>%
      clearControls() %>%
    addPolygons(data = layer_data,
                fillColor = ~pal(data_to_plot),
                fillOpacity = 0.7,
                weight = 2,
                opacity = .01,
                color = "white",
                dashArray = "3",
                highlightOptions = highlightOptions(
                  weight = .1,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = .7,
                  fillColor = "#666",
                  opacity = 1),
                label = labels,
                labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
                options = pathOptions(pane = "polygons")) %>%
      addPolylines(data = getNYCMap(),
                   weight = 1,
                   options = pathOptions(pane = "polylines")) %>%
      leaflet::addLegend("bottomright",pal = pal, values = legend_values,
                         opacity = 1, title = legend_title)
  })
  
  observe({
    if (input$mapType == "Default"){
      leafletProxy("mapPlot", data = getData()) %>%
        removeTiles("background_map") %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         layerId = "background_map",
                         # options = providerTileOptions(minZoom = 9, maxZoom = 12),
                         options = pathOptions(pane = "background_map"))
    } else if (input$mapType == "Night") {
      leafletProxy("mapPlot", data = getData()) %>%
        removeTiles("background_map") %>%
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
                         layerId = "background_map",
                         options = pathOptions(pane = "background_map"))
    } else {
      leafletProxy("mapPlot", data = getData()) %>%
        removeTiles("background_map") %>%
        addProviderTiles(providers$GeoportailFrance.orthos,
                         layerId = "background_map",
                         options = pathOptions(pane = "background_map"))
    }


  })
  
  # observe({
  #   leafletProxy("mapPlot", data = getData()) %>%
  #     clearControls() %>%
  #     leaflet::addLegend("bottomright",pal = pal, values = legend_values,
  #                               opacity = 1, title = legend_title)
  #     
  # })
  
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
  
  output$centralWeather <- renderFormattable({
    df <- getCentralParkData()
    table_df <- data.frame(matrix(nrow = 2, ncol = 8))
    table_df[,1] = c("Daily Max", "Daily Min")
    table_df[1,2:8] <- c_to_f(df$properties$maxTemperature$values$value[1:7])
    table_df[2,2:8] <- c_to_f(df$properties$minTemperature$values$value[1:7])
    # table_df = c_to_f(table_df)
    tim <- df$properties$minTemperature$values$validTime[1:7]
    dates <- format(as.Date(substring(tim, 1, 10),"%Y-%m-%d"),"%b %d")
    colnames(table_df)[2:8] <- dates
    colnames(table_df)[1] <- " "
    formattable(
      table_df
    )
  })
  
  output$centralWeatherHourly <- renderDygraph({
    df <- getCentralParkData()
    t <- paste(
      substring(df$properties$apparentTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$apparentTemperature$values$validTime, 12,19),
      sep = "")
    ft <- as.POSIXlt(t)
    plot_data <- data.frame(time = ft, temp = c_to_f(df$properties$apparentTemperature$values$value))
    # p <- ggplot(plot_dat, aes(x=time, y=temp)) +
    #   geom_line() + 
    #   xlab(" ") + ylab("Temperature") +
    #   theme(aspect.ratio=1/8)
    # p
    don <- xts(x = plot_data$temp, order.by = plot_data$time)
    p <- dygraph(don, width = 500, height = 100) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=F, 
                drawPoints = TRUE, pointSize = 2,
                fillAlpha=0.1, drawGrid = FALSE, 
                colors="#D8AE5A") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)
    p
  })
  
}