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
    api_call("34", "38")
  )
  
  getBronxData <- reactive(
    api_call("36", "41")
  )
  getQueensData <- reactive(
    api_call("39", "37")
  )
  
  getBrooklynData <- reactive(
    api_call("36", "33")
  )
  
  getStatenIslandData <- reactive(
    api_call("29", "29")
  )
  
  getWarningsData <-reactive(
    api_warning_call_2()
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
  
  output$warningMap <- renderLeaflet({
    alert_data <- getWarningsData()
    nyc_data <- getNYCMap()
    
    pal = c("Small Craft Advisory"="#d8bfd8",
            "Rip Current Statement"="#40e0d0",
            "Air Quality Alert"="#808080",
            "Marine Weather Statement"="#ffefd5",
            "Special Marine Warning"="#ffa500",
            "Coastal Flood Advisory"="#7cfc00",
            "Flood Watch"="#2e8b57",
            "Coastal Flood Statement"="#6b8e23",
            "Hazardous Weather Outlook"="#eee8aa")
    
    
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels)
    for (i in 1:length(alert_data$alerts)){
      labels <- sprintf(
        "<strong>%s</strong>",
        alert_data$alerts[[i]]
      ) %>% lapply(htmltools::HTML)
      m <- leaflet::addPolygons(m, data = alert_data$shapes[[i]],
                                fillColor = pal[[alert_data$alerts[[i]]]], fillOpacity = .9,
                                color = pal[[alert_data$alerts[[i]]]],
                                weight = 1,
                                label = labels,
                                labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "15px",
                                  direction = "auto")
      )
    }
    m <- addPolylines(m, data = nyc_data,
                      weight = .5, color = "blue")
    m
    
   
    
  })
  
  output$radarImage <- renderImage({
    # img(src="KOKX_loop.gif",height='500px')
    
    download.file("https://radar.weather.gov/ridge/standard/KOKX_loop.gif",
                  "www/KOKX_loop.gif", quiet = T, mode = 'wb')
    return(list(
      src = "www/KOKX_loop.gif",
      filetype = "www/gif",
      width = 530,
      alt = "National Weather Service Radar"
    ))
  }, deleteFile = F)
  
  output$satImage <- renderImage({
    download.file("https://cdn.star.nesdis.noaa.gov/GOES16/ABI/SECTOR/ne/GEOCOLOR/1200x1200.jpg",
                  "www/sat.jpg", quiet = T, mode = 'wb')
    return(list(
      src = "www/sat.jpg",
      filetype = "www/jpg",
      width = 530,
      alt = "National Weather Service Sattelite"
    ))
  }, deleteFile = F)
  
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
      #-20,-10, ..
      # pal <- colorNumeric(palette = c("#93b1d7", "#7f9bc3", "#607ba6",
      #                                 "#4d6591", "#39517f", "#26436f",
      #                                 "#275b80", "#287593", "#648d89",
      #                                 "#aba87d", "#c19d61", "#be704c",
      #                                 "#9f294c", "#6e1531", "#3d0216"), 
      #                     domain = c(120, -20), reverse = T)
      pal <- colorNumeric(palette = "Spectral", domain = c(120, -20), reverse = T)
      legend_title = "&degF"
      legend_values = c(120, -20)
    } 
    else if (field == "Relative Humidity") {
      data_to_plot = layer_data$rh
      labels <- sprintf(
        "<strong>%.0f&#37</strong>",
        layer_data$rh
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Greens", domain = c(1,100))
      legend_title <- "&#37"
      legend_values = c(1,100)
    } 
    else if (field == "Max. Temperature") {
      data_to_plot = c_to_f(layer_data$maxTemp)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Spectral", domain = c(120, -20), reverse = T)
      legend_title = "&degF"
      legend_values = c(120, -20)
    } 
    else if (field == "Min. Temperature") {
      data_to_plot = c_to_f(layer_data$minTemp)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = "Spectral", domain = c(120, -20), reverse = T)
      legend_title = "&degF"
      legend_values = c(120, -20)
    } 
    else if (field == "Dew Point"){
      data_to_plot = c_to_f(layer_data$dewpoint)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = "Spectral", domain = c(32, 80), reverse = T)
      legend_title = "&degF"
      legend_values = c(32, 80)
    }
    else if (field == "Heat Index"){
      data_to_plot = c_to_f(layer_data$heatIndex)
      labels <- sprintf(
        "<strong>%.0f&degF</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      pal <- colorNumeric(palette = "Spectral", domain = c(-20, 120), reverse = T)
      legend_title = "&degF"
      legend_values = c(-20, 120)
    }
    else if (field == "Sky Cover") {
      data_to_plot = layer_data$skyCover
      labels <- sprintf(
        "<strong>%.0f&#37</strong>",
        layer_data$rh
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Greys", domain = c(0,100))
      legend_title <- "&#37"
      legend_values = c(0,100)
    }
    else if (field == "Chance of Precipitation") {
      data_to_plot = layer_data$probabilityOfPrecipitation
      labels <- sprintf(
        "<strong>%.0f&#37</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Blues", domain = c(0,100))
      legend_title <- "&#37"
      legend_values = c(0,100)
    }
    else if (field == "Amt. of Precipitation") {
      data_to_plot = mm_to_in(layer_data$quantitativePrecipitation)
      labels <- sprintf(
        "<strong>%.2f in</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "BuPu", domain = c(0,8))
      legend_title <- "in"
      legend_values = c(0,8)
    }
    else if (field == "Ice Accumulation") {
      data_to_plot = mm_to_in(layer_data$iceAccumulation)
      labels <- sprintf(
        "<strong>%.2f in</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "BuPu", domain = c(0,8))
      legend_title <- "in"
      legend_values = c(0,8)
    }
    else if (field == "Snowfall Amount") {
      data_to_plot = mm_to_in(layer_data$snowfallAmount)
      labels <- sprintf(
        "<strong>%.2f in</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "BuPu", domain = c(0,8))
      legend_title <- "in"
      legend_values = c(0,8)
    }
    else if (field == "Visibility") {
      data_to_plot = m_to_mi(layer_data$visibility)
      labels <- sprintf(
        "<strong>%.0f mi</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Greys", domain = c(0,10), reverse = T)
      legend_title <- "mi"
      legend_values = c(0,10)
    }
    else if (field == "Lightning Activity") {
      data_to_plot = layer_data$lightningActivityLevel
      labels <- sprintf(
        "<strong>LAL%.0f</strong>",
        data_to_plot
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorNumeric(palette = "Greys", domain = c(0,10), reverse = T)
      pal <- colorBin(palette = "YlOrRd", bins = 6, domain = c(1,2,3,4,5,6))
      legend_title <- "LAL"
      legend_values = c(1,2,3,4,5,6)
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
                fillColor = pal(data_to_plot),
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
                         # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
  })
  
  observe({
    if (input$mapType == "Default"){
      leafletProxy("mapPlot", data = getData()) %>%
        removeTiles("background_map") %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels,
                         layerId = "background_map",
                         # options = providerTileOptions(minZoom = 9, maxZoom = 12),
                         options = pathOptions(pane = "background_map"))
      leafletProxy("warningMap", data = getData()) %>%
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
      leafletProxy("warningMap", data = getData()) %>%
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
      leafletProxy("warningMap", data = getData()) %>%
        removeTiles("background_map") %>%
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels,
                         layerId = "background_map",
                         options = pathOptions(pane = "background_map"))
    }


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
      substring(df$properties$temperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$temperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    
    plot_data <- data.frame(time = ft, Temperature = c_to_f(df$properties$temperature$values$value))
    
    t <- paste(
      substring(df$properties$maxTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$maxTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    mdata <- data.frame(time = ft, Max = c_to_f(df$properties$maxTemperature$values$value))
    
    plot_data <- merge(plot_data, mdata, by="time", all=T)
    plot_data$Max <- na.locf(plot_data$Max, na.rm = F, fromLast = F)
    
    t <- paste(
      substring(df$properties$minTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$minTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    mdata <- data.frame(time = ft, Min = c_to_f(df$properties$minTemperature$values$value))
    
    plot_data <- merge(plot_data, mdata, by="time", all=T)
    plot_data$Min <- na.locf(plot_data$Min, na.rm = F, fromLast = F)
    
    # maxTemp <- plot_data[plot_data$Temperature == max(plot_data$Temperature),]
    # minTemp <- plot_data[plot_data$Temperature == min(plot_data$Temperature),]
    
    # don <- xts(x = plot_data$temp, order.by = plot_data$time)
    
    p <- dygraph(plot_data, width = 500, height = 200, ylab = "Temperature &degF") %>%
      dyOptions(stepPlot = TRUE, drawGrid = F) %>%
      dyLegend(show = "follow", width = 350)
    #   dyAnnotation(maxTemp$time[1], text = paste(maxTemp$Temperature[1])) %>%
    # dyAnnotation(minTemp$time[1], text = paste(minTemp$Temperature[1]))
    p
  })
  
  output$warningText <- renderUI({ 
    df <- getWarningsData()
    if (length(df$alerts) == 0){
      s <- "There are no active watches, warnings, or advisories for New York City"
    } else {
      s <- ""
      for (i in 1:length(df$alerts)){
        # s <- HTML(paste(s, df$alerts[i], "<br/>", df$des[i], "newline"))
        s <- HTML(paste(s, df$alerts[i], "<br/>"))
      }
      return(s)
    }
    # return("Testing")
  })
  
  output$boroughManattanHourly <- renderDygraph({
    df <- getCentralParkData()
    t <- paste(
      substring(df$properties$apparentTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$apparentTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    
    plot_data <- data.frame(time = ft, Temperature = c_to_f(df$properties$apparentTemperature$values$value))
    
    
    
    maxTemp <- plot_data[plot_data$Temperature == max(plot_data$Temperature),]
    minTemp <- plot_data[plot_data$Temperature == min(plot_data$Temperature),]

    colnames(plot_data) <- c("time", "Heat Index")
    
    p <- dygraph(plot_data, width = 500, height = 200) %>%
      dyOptions(stepPlot = TRUE, drawGrid = F) %>%
      dyLegend(show = "follow") %>%
      dyAnnotation(maxTemp$time[1], text = paste(maxTemp$Temperature[1]), 
                   height = 18, width = 18, tickHeight = 6) %>%
      dyAnnotation(minTemp$time[1], text = paste(minTemp$Temperature[1]),
                   height = 18, width = 18, tickHeight = 14)
    p
  })
  output$boroughBronxHourly <- renderDygraph({
    df <- getBronxData()
    t <- paste(
      substring(df$properties$apparentTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$apparentTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)

    plot_data <- data.frame(time = ft, HI = c_to_f(df$properties$apparentTemperature$values$value))
    maxTemp <- plot_data[plot_data$HI == max(plot_data$HI),]
    minTemp <- plot_data[plot_data$HI == min(plot_data$HI),]
    
    colnames(plot_data) <- c("time", "Heat Index")
    
    
    
    p <- dygraph(plot_data, width = 500, height = 200) %>%
      dyOptions(stepPlot = TRUE, drawGrid = F) %>%
      dyLegend(show = "follow") %>%
      dyAnnotation(maxTemp$time[1], text = paste(maxTemp$HI[1]), 
                 height = 18, width = 18, tickHeight = 6) %>%
      dyAnnotation(minTemp$time[1], text = paste(minTemp$HI[1]),
                   height = 18, width = 18, tickHeight = 14)
    p
  })
  output$boroughBrooklynHourly <- renderDygraph({
    df <- getBrooklynData()
    t <- paste(
      substring(df$properties$apparentTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$apparentTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    
    plot_data <- data.frame(time = ft, HI = c_to_f(df$properties$apparentTemperature$values$value))
    maxTemp <- plot_data[plot_data$HI == max(plot_data$HI),]
    minTemp <- plot_data[plot_data$HI == min(plot_data$HI),]
    colnames(plot_data) <- c("time", "Heat Index")
    
    p <- dygraph(plot_data, width = 500, height = 200) %>%
      dyOptions(stepPlot = TRUE, drawGrid = F) %>%
      dyLegend(show = "follow") %>%
      dyAnnotation(maxTemp$time[1], text = paste(maxTemp$HI[1]), 
                 height = 18, width = 18, tickHeight = 6) %>%
      dyAnnotation(minTemp$time[1], text = paste(minTemp$HI[1]),
                   height = 18, width = 18, tickHeight = 14)
    p
  })
  output$boroughQueensHourly <- renderDygraph({
    df <- getQueensData()
    t <- paste(
      substring(df$properties$apparentTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$apparentTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    
    plot_data <- data.frame(time = ft, HI = c_to_f(df$properties$apparentTemperature$values$value))
    maxTemp <- plot_data[plot_data$HI == max(plot_data$HI),]
    minTemp <- plot_data[plot_data$HI == min(plot_data$HI),]
    colnames(plot_data) <- c("time", "Heat Index")
    
    p <- dygraph(plot_data, width = 500, height = 200) %>%
      dyOptions(stepPlot = TRUE, drawGrid = F) %>%
      dyLegend(show = "follow") %>%
      dyAnnotation(maxTemp$time[1], text = paste(maxTemp$HI[1]), 
                 height = 18, width = 18, tickHeight = 6) %>%
      dyAnnotation(minTemp$time[1], text = paste(minTemp$HI[1]),
                   height = 18, width = 18, tickHeight = 14)
    p
  })
  output$boroughStatenIslandHourly <- renderDygraph({
    df <- getStatenIslandData()
    t <- paste(
      substring(df$properties$apparentTemperature$values$validTime, 1, 10),
      " ",
      substring(df$properties$apparentTemperature$values$validTime, 12,16),
      sep = "")
    ft <- as.POSIXlt(t)
    
    plot_data <- data.frame(time = ft, HI = c_to_f(df$properties$apparentTemperature$values$value))
    maxTemp <- plot_data[plot_data$HI == max(plot_data$HI),]
    minTemp <- plot_data[plot_data$HI == min(plot_data$HI),]
    colnames(plot_data) <- c("time", "Heat Index")
    
    p <- dygraph(plot_data, width = 500, height = 200) %>%
      dyOptions(stepPlot = TRUE, drawGrid = F) %>%
      dyLegend(show = "follow") %>%
      dyAnnotation(maxTemp$time[1], text = paste(maxTemp$HI[1]), 
                 height = 18, width = 18, tickHeight = 6) %>%
      dyAnnotation(minTemp$time[1], text = paste(minTemp$HI[1]),
                   height = 18, width = 18, tickHeight = 14)
    p
  })
  
  output$heatHazardMap <- renderLeaflet({
    layer_data = getData()
    data_to_plot = c_to_f(layer_data$heatIndex)
    labels <- sprintf(
      "<strong>%.0f&degF</strong>",
      data_to_plot
    ) %>% lapply(htmltools::HTML)
    pal <- colorNumeric(palette = "Spectral", domain = c(50, 120), reverse = T)
    legend_title = "&degF"
    legend_values = c(50, 120)
    
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
      addPolygons(data = layer_data,
                  fillColor = pal(data_to_plot),
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
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, 
                       options = pathOptions(pane = "labels")) %>%
      leaflet::addLegend("bottomright",pal = pal, values = legend_values,
                         opacity = 1, title = legend_title)

    m
  })
  
  
  
}