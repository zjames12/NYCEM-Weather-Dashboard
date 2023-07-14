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
  return(df * 9.0 / 5.0 + 32)
}

mm_to_in <- function(df) {
  return(df / 25.4)
}

m_to_mi <-function(df) {
  return(df / 1609.344)
}

api_call <- function(X, Y, num_attempts = 10){
  # query <- "https://api.weather.gov/gridpoints/OKX/34,38"
  query <- paste("https://api.weather.gov/gridpoints/OKX/", X, ",", Y, sep = "")
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
  dewpoint_data <- matrix(ncol = 30, nrow = num_grids)
  wetBulbGlobeTemperature_data <- matrix(ncol = 60, nrow = num_grids)
  heatIndex_data <- matrix(ncol = 60, nrow = num_grids)
  windChill_data <- matrix(ncol = 60, nrow = num_grids)
  skyCover_data <- matrix(ncol = 60, nrow = num_grids)
  windDirection_data <- matrix(ncol = 60, nrow = num_grids)
  windSpeed_data <- matrix(ncol = 30, nrow = num_grids)
  windGust_data <- matrix(ncol = 60, nrow = num_grids)
  probabilityOfPrecipitation_data <- matrix(ncol = 30, nrow = num_grids)
  quantitativePrecipitation_data <- matrix(ncol = 10, nrow = num_grids)
  iceAccumulation_data <- matrix(ncol = 10, nrow = num_grids)
  snowfallAmount_data <- matrix(ncol = 10, nrow = num_grids)
  visibility_data <- matrix(ncol = 3, nrow = num_grids)
  lightningActivityLevel_data <- matrix(ncol = 2, nrow = num_grids)
  # pressure_data 
  # probabilityOfTropicalStormWinds_data
  # probabilityOfHurricane_data
  # potentialOf15mphWinds_data
  # potentialOf25mphWinds_data
  # potentialOf35mphWinds_data
  # potentialOf45mphWinds_data
  # potentialOf20mphWinds_data
  # potentialOf30mphWinds_data
  # potentialOf40mphWinds_data
  # potentialOf50mphWinds_data
  # potentialOf60mphWinds_data
  # probabilityOfThunder_data
  # redFlagThreatIndex_data
  
  init = F
  success_cnt = 0
  poly = list()
  # pb = txtProgressBar(min = 0, max = length(length(grid_points$X)), initial = 0)
  for (i in 1:(length(grid_points$X))){#length(grid_points$X)
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
      maxTemperature = raw_data$properties$maxTemperature$values$value
      minTemperature = raw_data$properties$minTemperature$values$value
      apparentTemperature = raw_data$properties$apparentTemperature$values$value
      
      dewpoint = raw_data$properties$dewpoint$values$value
      wetBulbGlobeTemperature = raw_data$properties$wetBulbGlobeTemperature$values$value
      heatIndex = raw_data$properties$heatIndex$values$value
      windChill = raw_data$properties$windChill$values$value
      skyCover = raw_data$properties$maxTemperature$values$value
      windDirection = raw_data$properties$windDirection$values$value
      windSpeed = raw_data$properties$windSpeed$values$value
      windGust = raw_data$properties$windGust$values$value
      probabilityOfPrecipitation = raw_data$properties$probabilityOfPrecipitation$values$value
      quantitativePrecipitation = raw_data$properties$quantitativePrecipitation$values$value
      iceAccumulation = raw_data$properties$iceAccumulation$values$value
      snowfallAmount = raw_data$properties$snowfallAmount$values$value
      visibility = raw_data$properties$visibility$values$value
      lightningActivityLevel = raw_data$properties$lightningActivityLevel$values$value

      sr = Polygon(cbind(raw_data$geometry$coordinates[,,1], 
                         raw_data$geometry$coordinates[,,2]))
      
      srs = Polygons(list(sr), paste(i))
      poly = append(poly, srs)
      # print(length(temp))


      temp_data[i,] = temp[1:60]
      rh_data[i,] = rh[1:60]
      minTemperature_data[i,] = minTemperature[1:8]
      maxTemperature_data[i,] = maxTemperature[1:8]
      dewpoint_data[i,] <- dewpoint[1:30]
      # wetBulbGlobeTemperature_data[i,] <- wetBulbGlobeTemperature[1:60]
      heatIndex_data[i,] <- heatIndex[1:60]
      # windChill_data[i,] <- windChill[1:60]
      
      skyCover_data[i,] <- skyCover[1:60]
      windDirection_data[i,] <- windDirection[1:60]
      windSpeed_data[i,] <- windSpeed[1:30]
      windGust_data[i,] <- windGust[1:60]
      probabilityOfPrecipitation_data[i,] <- probabilityOfPrecipitation[1:30]
      quantitativePrecipitation_data[i,] <- quantitativePrecipitation[1:10]
      iceAccumulation_data[i,] <- iceAccumulation[1:10]
      snowfallAmount_data[i,] <- snowfallAmount[1:10]
      visibility_data[i,] <- visibility[1:3]
      lightningActivityLevel_data[i,] <- lightningActivityLevel[1:2]

      success_cnt = success_cnt + 1
    }
    # setTxtProgressBar(pb,i)
  }

  sp <- SpatialPolygons(poly, 1:success_cnt)
  dfd <- data.frame(temp_data[,1],
                    rh_data[,1], 
                    maxTemperature_data[,1], 
                    minTemperature_data[,1],
                    dewpoint_data[,1],
                    heatIndex_data[,1],
                    skyCover_data[,1],
                    probabilityOfPrecipitation_data[,1],
                    quantitativePrecipitation_data[,1],
                    iceAccumulation_data[,1],
                    snowfallAmount_data[,1],
                    visibility_data[,1],
                    lightningActivityLevel_data[,1]
                    )
  
  dfdt <- dfd[!is.na(dfd[,1]),]
  
  colnames(dfdt) <- c("temp", "rh",
                      "maxTemp", "minTemp",
                      "dewpoint",
                      "heatIndex", "skyCover","probabilityOfPrecipitation",
                      "quantitativePrecipitation", "iceAccumulation",
                      "snowfallAmount", "visibility",
                      "lightningActivityLevel")

  Xval <- grid_points$X[!is.na(dfd[,1])]
  Yval <- grid_points$Y[!is.na(dfd[,1])]
  
  sps <- SpatialPolygonsDataFrame(sp, data.frame(dfdt, X = Xval, Y=Yval, 
                                             row.names = row.names(sp)))
  return(sps)
}

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

radar_gif <- function(){
  download.file("https://radar.weather.gov/ridge/standard/KOKX_loop.gif",
                "www/KOKX_loop.gif")
  img(src="www/KOKX_loop.gif")
}

api_alerts_plot <- function(alert_data, nyc_data){
  pal = c("Small Craft Advisory"="#d8bfd8",
          "Rip Current Statement"="#40e0d0",
          "Air Quality Alert"="#808080",
          "Marine Weather Statement"="#ffefd5",
          "Special Marine Warning"="#ffa500")
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels)
  for (i in 1:length(alert_data$alerts)){
    print(i)
    m <- leaflet::addPolygons(m, data = alert_data$shapes[[i]],
                              fillColor = pal[[alert_data$alerts[[i]]]], fillOpacity = .9,
                              color = "white", weight = .5,
                              )
  }
  m <- addPolylines(m, data = nyc_data,
                    weight = .5, color = "blue")
  return(m)
}

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
    raw_data = NA
    while (length(raw_data) == 1 && cnt < 2) {
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

api_warning_call_2 <- function(){
  zones = c("NYZ072", "NYZ073", "NYZ074", "NYZ075", "NYZ176", "NYZ178",
            "ANZ335", "ANZ338", "ANZ355")
  alerts = c()
  shaps = c()
  query <- "https://api.weather.gov/alerts/active?zone=NYZ072,NYZ073,NYZ074,NYZ075,NYZ176,NYZ178,ANZ335,ANZ355,ANZ338"
  cnt = 1
  raw_data = NA
  while (length(raw_data) == 1 && cnt < 8) {
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
  if (is.null(names(raw_data$features$geometry))){
    num_alerts = length(raw_data$features$geometry)
    check = raw_data$features$geometry
  } else {
    num_alerts = length(raw_data$features$geometry$type)
    check = raw_data$features$geometry$type
  }
  
  poly = list()
  for (i in 1:num_alerts){
    # print(raw_data$features$geometry$type[[i]])
    if (is.na(check[[i]])){
      aff_area <- raw_data$features$properties$geocode$UGC[[i]]
      aff_area <- intersect(aff_area, zones)
      if (length(aff_area) != 0){
        a <- NA
        cnt <- 1
        for (j in 1:length(aff_area)){
          if (!is.na(aff_area[j])) {
            fi = paste("shape/",aff_area[j], ".geojson", sep="")
            sr <- geojson_read(fi,  what = "sp")
            if (cnt == 1){
              a <- sr
              cnt = 2
            } else {
              a <- rbind(a, sr)
            }
            # srs <- sr@polygons
            # sr = Polygon(cbind(raw_data$geometry$coordinates[,,1], raw_data$geometry$coordinates[,,2]))
            # srs = Polygons(list(sr), paste(i))
            
            
          }
        }
        poly = append(poly, a)
      } else {
        print("Error")
      } 
      
    } else {
      # a = raw_data$features$geometry[i]
      # raw_data$features$geometry$coordinates[[i]]
      
      sr = Polygon(cbind(raw_data$features$geometry$coordinates[[i]][,,1],
                         raw_data$features$geometry$coordinates[[i]][,,2]))

      poly = append(poly, sr)
    }
  }
  
  # al <- raw_data$features$properties$event[is.na(raw_data$features$geometry$type)]
  al <- raw_data$features$properties$event
  return(list(alerts=al, shapes=poly, des=raw_data$features$properties$description))
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
