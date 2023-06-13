suppressWarnings(suppressMessages(library(RJSONIO, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(RCurl, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(jsonlite, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(ggspatial, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(geojsonio, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(broom, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(lubridate, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(TSstudio, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(ggplot2, quietly = T, warn.conflicts = F)))
suppressWarnings(suppressMessages(library(doParallel, quietly = T, warn.conflicts = F)))
library(leaflet)
library(sp)
library(RColorBrewer)
library(dygraphs)
library(xts)

# 34, 38
nyc_outline <- function(){
  spdf <- geojson_read("Borough Boundaries.geojson",  what = "sp")
  # spdf_fortified <- tidy(spdf)
  return(spdf)
}

c_to_f <- function(df){
  return(df * 9 / 5 + 32)
}

api_call_central_park <- function(num_attempts = 10){
  query <- "https://api.weather.gov/gridpoints/OKX/34,38"
  cnt = 1
  raw_data = NA
  while (length(raw_data) == 1 && cnt < num_attempts) {
    raw_data <- tryCatch(
      {
        jsonlite::fromJSON(query)
      },
      warning=function(cond) {
        suppressWarnings({
          # message(paste("URL caused a warning:", url))
          # message("Here's the original warning message:")
          # message(cond)
          # Choose a return value in case of warning
          return(NA)
        })
      },
      error=function(cond) {
        # message(paste("URL does not seem to exist:", query))
        # message("Here's the original error message:")
        # message(cond)
        # Choose a return value in case of error
        return(NA)
      })
    cnt = cnt + 1
  }
  return(raw_data)
}

call_api_2 <- function(){

  grid_points = read.csv("nyc_grid_locs.csv")

  num_grids = length(grid_points$X)
  temp_data <- matrix(ncol = 60, nrow = num_grids)
  maxTemperature_data <- matrix(ncol = 8, nrow = num_grids)
  minTemperature_data <- matrix(ncol = 8, nrow = num_grids)
  rh_data <- matrix(ncol = 60, nrow = num_grids)
  apparentTemperature_data <- matrix(ncol = 60, nrow = num_grids)
  # wetBulbGlobeTemperature_data <- matrix(ncol = 60, nrow = num_grids)
  heatIndex_data <- matrix(ncol = 60, nrow = num_grids)
  # windChill_data <- matrix(ncol = 60, nrow = num_grids)
  skyCover_data <- matrix(ncol = 60, nrow = num_grids)
  windDirection_data <- matrix(ncol = 60, nrow = num_grids)
  windSpeed_data <- matrix(ncol = 60, nrow = num_grids)
  windGust_data <- matrix(ncol = 60, nrow = num_grids)
  probabilityOfPrecipitation_data <- matrix(ncol = 30, nrow = num_grids)
  quantitativePrecipitation_data <- matrix(ncol = 20, nrow = num_grids)
  iceAccumulation_data <- matrix(ncol = 30, nrow = num_grids)
  snowfallAmount_data <- matrix(ncol = 30, nrow = num_grids)
  # snowLevel_data <- matrix(ncol = 60, nrow = num_grids)
  # ceilingHeight_data <- matrix(ncol = 60, nrow = num_grids)
  # visibility_data <- matrix(ncol = 60, nrow = num_grids)
  transportWindSpeed_data <- matrix(ncol = 60, nrow = num_grids)
  transportWindDirection_data <- matrix(ncol = 60, nrow = num_grids)
  mixingHeight_data <- matrix(ncol = 60, nrow = num_grids)
  # hainesIndex_data <- matrix(ncol = 60, nrow = num_grids)
  lightningActivityLevel_data <- matrix(ncol = 2, nrow = num_grids)
  
  init = F
  success_cnt = 0
  poly = list()
  # pb = txtProgressBar(min = 0, max = length(length(grid_points$X)), initial = 0)
  for (i in 1:(length(grid_points$X))){
    gridX = grid_points[i, 1]
    gridY = grid_points[i, 2]
    query <- paste("https://api.weather.gov/gridpoints/OKX/", as.character(gridX), ",", as.character(gridY), sep = "")
    raw_data = NA
    # print(query)
    # check = F
    # raw_data = api_call(query, check)
    time = NA
    cnt = 1
    # 
    # 
    while (length(raw_data) == 1 && cnt < 4) {
      raw_data <- tryCatch(
        {
          jsonlite::fromJSON(query)
        },
        warning=function(cond) {
          suppressWarnings({
            # message(paste("URL caused a warning:", url))
            # message("Here's the original warning message:")
            # message(cond)
            # Choose a return value in case of warning
            return(NA)
          })
        },
        error=function(cond) {
          # message(paste("URL does not seem to exist:", query))
          # message("Here's the original error message:")
          # message(cond)
          # Choose a return value in case of error
          return(NA)
        })
      cnt = cnt + 1
    }
    if (length(raw_data) != 1){
      temp = raw_data$properties$temperature$values$value

      rh = raw_data$properties$relativeHumidity$values$value
      rh_time = raw_data$properties$relativeHumidity$values$validTime


      maxTemperature = raw_data$properties$maxTemperature$values$value
      # maxTemperature_data = raw_data$properties$maxTemperature$values$validTime


      minTemperature = raw_data$properties$minTemperature$values$value
      apparentTemperature = raw_data$properties$apparentTemperature$values$value
      wetBulbGlobeTemperature = raw_data$properties$maxTemperature$values$value
      heatIndex = raw_data$properties$maxTemperature$values$value
      windChill = raw_data$properties$maxTemperature$values$value
      skyCover = raw_data$properties$maxTemperature$values$value
      windDirection = raw_data$properties$maxTemperature$values$value
      windSpeed = raw_data$properties$maxTemperature$values$value
      windGust = raw_data$properties$maxTemperature$values$value
      probabilityOfPrecipitation = raw_data$properties$maxTemperature$values$value
      quantitativePrecipitation = raw_data$properties$maxTemperature$values$value
      iceAccumulation = raw_data$properties$maxTemperature$values$value
      snowfallAmount = raw_data$properties$maxTemperature$values$value
      snowLevel = raw_data$properties$maxTemperature$values$value
      ceilingHeight = raw_data$properties$maxTemperature$values$value
      visibility = raw_data$properties$maxTemperature$values$value
      transportWindSpeed = raw_data$properties$maxTemperature$values$value
      transportWindDirection = raw_data$properties$maxTemperature$values$value
      mixingHeight = raw_data$properties$maxTemperature$values$value
      hainesIndex = raw_data$properties$maxTemperature$values$value
      lightningActivityLevel = raw_data$properties$maxTemperature$values$value

      sr = Polygon(cbind(raw_data$geometry$coordinates[,,1], raw_data$geometry$coordinates[,,2]))
      srs = Polygons(list(sr), paste(i))
      poly = append(poly, srs)
      # print(length(temp))


      temp_data[i,] = temp[1:60]
      rh_data[i,] = rh[1:60]
      minTemperature_data[i,] = minTemperature[1:8]
      maxTemperature_data[i,] = maxTemperature[1:8]

      success_cnt = success_cnt + 1
    }
    # setTxtProgressBar(pb,i)
  }

  sp <- SpatialPolygons(poly, 1:success_cnt)
  dfd <- data.frame(temp_data[,1],rh_data[,1], 
                    maxTemperature_data[,1], minTemperature_data[,1])
  dfdt <- dfd[!is.na(dfd[,1]),]
  colnames(dfdt) <- c("temp", "rh",
                      "maxTemp", "minTemp")

  Xval <- grid_points$X[!is.na(dfd[,1])]
  Yval <- grid_points$Y[!is.na(dfd[,1])]
  
  sps <- SpatialPolygonsDataFrame(sp, 
                                  data.frame(dfdt, X = Xval, Y=Yval, 
                                             row.names = row.names(sp)))
  # ret <- list(temp_data, rh_data, sps)
  # names(ret) <- c("temp", "rh_data", "poly")
  return(sps)
}

# plot_map <- function(nyc_data, layer_data, field){
#   # date = format(as.Date(substring(layer_data$time, 1, 10),"%Y-%m-%d"),"%b %d %Y")
#   if (field == "temp"){
#     c = layer_data$temp[,1]
#   } else if (field == "rh"){
#     c = layer_data$rh[,1]
#   }
#   map <- ggplot() +
#     geom_polygon(data = layer_data$layers, aes( x = long, y = lat, group = group, fill = c, color = c)) +
#     geom_polygon(data = nyc_data, aes( x = long, y = lat, group = group), fill= alpha("#2C77BF", 0), color="white") +
#     theme_void() +
#     scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
#     scale_color_gradient(low = "blue", high = "red", na.value = NA) +
#     labs(fill = "\u00B0C", color = "\u00B0C") +
#     # ggtitle(paste("Temperature \n", date, " ", substring(layer_data$time, 12,19), sep = "")) +
#     coord_map()
#   return(map)
# }

api_data_plot <- function(layer_data, nyc_data, field){
  if (field == "Temperature"){
    data_to_plot = layer_data$temp
    labels <- sprintf(
      "<strong>%.0f&degC</strong>",
      layer_data$temp
    ) %>% lapply(htmltools::HTML)

    # bins <- 0:11 *.5 + 14
    # pal <- colorBin("Spectral", domain = layer_data$temp, bins = bins)
    pal <- colorNumeric(palette = "Spectral", domain = data_to_plot, reverse = F)
    legend_title = "&degC"
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
      "<strong>%.0f&degC</strong>",
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
  m <- leaflet() %>%
    addMapPane("background_map", zIndex = 410) %>%  
    addMapPane("polygons", zIndex = 420) %>%        
    addMapPane("polylines", zIndex = 430) %>%
    addMapPane("labels", zIndex = 440) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels, #GeoportailFrance.orthos, 
                     # options = providerTileOptions(minZoom = 9, maxZoom = 12),
                     options = pathOptions(pane = "background_map")) %>%
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
                options = pathOptions(pane = "polygons"))%>%
      addPolylines(data = nyc_data,
                  weight = 1, options = pathOptions(pane = "polylines")) %>%
      addProviderTiles(providers$CartoDB.PositronOnlyLabels, #GeoportailFrance.orthos, 
                     # options = providerTileOptions(minZoom = 9, maxZoom = 12),
                     options = pathOptions(pane = "labels")) %>%
    
      leaflet::addLegend("bottomright",pal = pal, values = legend_values,
              opacity = 1, title = legend_title)
  return(m)

}

api_alerts_plot <- function(alert_data, nyc_data){
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels)
  for (i in 1:length(alert_data$alerts)){
    
    sh <- geojson_read(alert_data$shapes[i],  what = "sp")
    m <- leaflet::addPolygons(m, data = sh)
  }
  return(m)
}

# plot_rh_map <- function(nyc_data, layer_data){
#   date = format(as.Date(substring(layer_data$time, 1, 10),"%Y-%m-%d"),"%b %d %Y")
#   map <- ggplot() +
#     geom_polygon(data = layer_data$layers, aes( x = long, y = lat, group = group, fill = rh, color = rh)) +
#     geom_polygon(data = nyc_data, aes( x = long, y = lat, group = group), fill= alpha("#2C77BF", 0), color="white") +
#     theme_void() +
#     scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
#     scale_color_gradient(low = "blue", high = "red", na.value = NA) +
#     labs(fill = "%", color = "%") +
#     ggtitle(paste("Relative Humidity \n", date, " ", substring(layer_data$time, 12,19), sep = "")) +
#     coord_map()
#   return(map)
# }

plot_city_hourly <- function(layer_data) {
  t <- paste(substring(layer_data$time, 1, 10), " ", substring(layer_data$time, 12,19), sep = "")
  t <- as.POSIXlt(strptime(t, "%Y-%m-%d %H:%M:%OS"))
  time_list <- rep(t, 156)
  for (i in 1:156){
    time_list[i] = t + hours(i - 1)
  }
  time_series <- data.frame(time_list, layer_data$city_hourly)
  colnames(time_series) <- c("time", "val")
  # tplot <- ts_plot(time_series, title = "7 Day Forcast", Ytitle = "Temperature \u00B0F")
  # tplot <- hist(layer_data$city_hourly)
  p <- ggplot(time_series, aes(x=time, y=val)) +
    geom_line() +
    xlab("") +
    ylab("Temperature \u00B0F") +
    ggtitle("7 Day Forcast") +
    theme(aspect.ratio=1/4)
  return(p)

}

api_warning_call <- function(){
  zones = c("NYZ072", "NYZ073", "NYZ074", "NYZ075", "NYZ176", "NYZ178")
  alerts = c()
  shaps = c()
  for (i in 1:6){
    query <- paste("https://api.weather.gov/alerts/active?zone=", zones[i], sep = "")
    cnt = 1
    
    while (length(raw_data) == 1 && cnt < 4) {
      raw_data <- tryCatch(
        {
          jsonlite::fromJSON(query)
        },
        warning=function(cond) {
          suppressWarnings({
            # message(paste("URL caused a warning:", url))
            # message("Here's the original warning message:")
            # message(cond)
            # Choose a return value in case of warning
            return(NA)
          })
        },
        error=function(cond) {
          # message(paste("URL does not seem to exist:", query))
          # message("Here's the original error message:")
          # message(cond)
          # Choose a return value in case of error
          return(NA)
        })
      cnt = cnt + 1
    }
    if (length(raw_data) != 1 && length(raw_data$features) != 0){
      mes <- raw_data$features$properties$event[1]
      shape <- paste("shape/", zones[i], ".geojson", sep = "")
      alerts = append(alerts, mes)
      shaps = append(shaps, shape)
    }
  }
  return(list(alerts=alerts, shapes=shaps))
}



# df <- api_warning_call()

# nyc_data <- nyc_outline()
# t <- proc.time()
# layer_data <- call_api_2()
# e <- proc.time()
# e - t
# api_data_plot(layer_data, nyc_data, "Temperature")


#
# lon = 26:44
# lat = 24:44
# lonlat <- as.data.frame(expand.grid(lon,lat))
# colnames(lonlat) <- c("X", "Y")
# lonlat <- lonlat[!(lonlat$X <= 31 & lonlat$Y >= 33) & !(lonlat$X >= 32 & lonlat$Y <= 27),]
# write.table(lonlat, "nyc_grid_locs.csv", row.names = F, sep = ",")
