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

nyc_outline <- function(){
  spdf <- geojson_read("Borough Boundaries.geojson",  what = "sp")
  spdf_fortified <- tidy(spdf)
  return(spdf_fortified)
}

call_api <- function(){
  
  
  gridX = 26
  gridY = 34
  sh <- data.frame(matrix(ncol = 7, nrow = 0))
  # !(gridX <= 31 && gridY >= 24)
  for (i in 1:18){
    gridX = gridX + 1
    gridY = 44
    for (j in 1:20){
      gridY = gridY - 1
      if (T){ #To be modified to select which grids to display
        query <- paste("https://api.weather.gov/gridpoints/OKX/", as.character(gridX), ",", as.character(gridY) , "/forecast", sep = "")
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
        if (length(raw_data) != 1){
          temp = raw_data$properties$periods[["temperature"]][1]
          rh = raw_data$properties$periods$relativeHumidity$value[1]
          id = gridX * 100 + gridY
          order_start = 36123 + (i - 1) * 5
          order_end = order_start + 4
          df <- data.frame(raw_data$geometry$coordinates[,,1], raw_data$geometry$coordinates[,,2],c(1:5), rep(FALSE, 5),rep(23+i, 5),as.factor(rep(i+5, 5)),rep(i + 5, 5),rep(temp,5),rep(rh,5))
          colnames(df) <- c("long", "lat", "order", "hole", "piece", "group", "id", "temp", "rh")
          
          sh <- rbind(sh, df)
        }
      }
    }
  }
  colnames(sh) <- c("long", "lat", "order", "hole", "piece", "group", "id", "temp")
  return(sh)
}



call_api_2 <- function(){
  
  grid_points = read.csv("nyc_grid_locs.csv")
  sh <- data.frame(matrix(ncol = 7, nrow = 0))
  city_hourly = rep(0, 156) # magic number?
  success_cnt = 0
  for (i in 1:length(grid_points$X)){
    gridX = grid_points[i, 1]
    gridY = grid_points[i, 2]
    query <- paste("https://api.weather.gov/gridpoints/OKX/", as.character(gridX), ",", as.character(gridY) , "/forecast/hourly", sep = "")
    raw_data = NA
    time = NA
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
    if (length(raw_data) != 1){
      temp = raw_data$properties$periods[["temperature"]][1]
      city_hourly = raw_data$properties$periods$temperature + city_hourly
      # print(query)
      # print(temp)
      # print(raw_data$properties$periods$temperature)
      rh = raw_data$properties$periods$relativeHumidity$value[1]
      time = raw_data$properties$periods$startTime[1]
      # id = gridX * 100 + gridY
      # order_start = 36123 + (i - 1) * 5
      # order_end = order_start + 4
      df <- data.frame(raw_data$geometry$coordinates[,,1], raw_data$geometry$coordinates[,,2],c(1:5), rep(FALSE, 5),rep(23+i, 5),as.factor(rep(i+5, 5)),rep(i + 5, 5),rep(temp,5), rep(rh,5))
      colnames(df) <- c("long", "lat", "order", "hole", "piece", "group", "id", "temp")
      
      sh <- rbind(sh, df)
      success_cnt = success_cnt + 1
    }
  }
  print(success_cnt)
  city_hourly = city_hourly / success_cnt
  colnames(sh) <- c("long", "lat", "order", "hole", "piece", "group", "id", "temp", "rh")
  ret <- list(sh, time, city_hourly)
  names(ret) <- c("layers", "time", "city_hourly")
  return(ret)
}

# call_api_parallel <- function(){
#   
#   grid_points = read.csv("nyc_grid_locs.csv")
#   sh <- data.frame(matrix(ncol = 7, nrow = 0))
#   city_hourly = rep(0, 156) # magic number?
#   success_cnt = 0
#   cl <- makePSOCKcluster(2)
#   registerDoParallel(cl)
#   foreach (i = 1:length(grid_points$X)) %dopar% {
#     gridX = grid_points[i, 1]
#     gridY = grid_points[i, 2]
#     query <- paste("https://api.weather.gov/gridpoints/OKX/", as.character(gridX), ",", as.character(gridY) , "/forecast/hourly", sep = "")
#     raw_data = NA
#     time = NA
#     cnt = 1
#     
#     while (length(raw_data) == 1 && cnt < 4) {
#       raw_data <- tryCatch(
#         {
#           jsonlite::fromJSON(query)
#         },
#         warning=function(cond) {
#           suppressWarnings({
#             # message(paste("URL caused a warning:", url))
#             # message("Here's the original warning message:")
#             # message(cond)
#             # Choose a return value in case of warning
#             return(NA)
#           })
#         },
#         error=function(cond) {
#           # message(paste("URL does not seem to exist:", query))
#           # message("Here's the original error message:")
#           # message(cond)
#           # Choose a return value in case of error
#           return(NA)
#         })
#       cnt = cnt + 1
#     }
#     if (length(raw_data) != 1){
#       temp = raw_data$properties$periods[["temperature"]][1]
#       # city_hourly = raw_data$properties$periods$temperature + city_hourly
#       # print(query)
#       # print(temp)
#       # print(raw_data$properties$periods$temperature)
#       rh = raw_data$properties$periods$relativeHumidity$value[1]
#       time = raw_data$properties$periods$startTime[1]
#       # id = gridX * 100 + gridY
#       # order_start = 36123 + (i - 1) * 5
#       # order_end = order_start + 4
#       df <- data.frame(raw_data$geometry$coordinates[,,1],
#                        raw_data$geometry$coordinates[,,2],c(1:5),
#                        rep(FALSE, 5),rep(23+i, 5),
#                        as.factor(rep(i+5, 5)),
#                        rep(i + 5, 5),rep(temp,5), rep(rh,5))
#       colnames(df) <- c("long", "lat", "order", "hole", "piece", "group", "id", "temp", "rh")
#       
#       sh <- rbind(sh, df)
#       success_cnt = success_cnt + 1
#       # sh
#     }
#   }
#   print(success_cnt)
#   city_hourly = city_hourly / success_cnt
#   colnames(sh) <- c("long", "lat", "order", "hole", "piece", "group", "id", "temp", "rh")
#   ret <- list(sh, time, city_hourly)
#   names(ret) <- c("layers", "time", "city_hourly")
#   return(ret)
# }


plot_temp_map <- function(nyc_data, layer_data){
  date = format(as.Date(substring(layer_data$time, 1, 10),"%Y-%m-%d"),"%b %d %Y")
  map <- ggplot() +
    geom_polygon(data = layer_data$layers, aes( x = long, y = lat, group = group, fill = temp, color = temp)) +
    geom_polygon(data = nyc_data, aes( x = long, y = lat, group = group), fill= alpha("#2C77BF", 0), color="white") +
    theme_void() +
    scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
    scale_color_gradient(low = "blue", high = "red", na.value = NA) +
    labs(fill = "\u00B0F", color = "\u00B0F") +
    ggtitle(paste("Temperature \n", date, " ", substring(layer_data$time, 12,19), sep = "")) +
    coord_map()
  return(map)
}

plot_rh_map <- function(nyc_data, layer_data){
  date = format(as.Date(substring(layer_data$time, 1, 10),"%Y-%m-%d"),"%b %d %Y")
  map <- ggplot() +
    geom_polygon(data = layer_data$layers, aes( x = long, y = lat, group = group, fill = rh, color = rh)) +
    geom_polygon(data = nyc_data, aes( x = long, y = lat, group = group), fill= alpha("#2C77BF", 0), color="white") +
    theme_void() +
    scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
    scale_color_gradient(low = "blue", high = "red", na.value = NA) +
    labs(fill = "%", color = "%") +
    ggtitle(paste("Relative Humidity \n", date, " ", substring(layer_data$time, 12,19), sep = "")) +
    coord_map()
  return(map)
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

# nyc <- nyc_outline()
# t <- proc.time()
# layer_data <- call_api_2()
# e <- proc.time()
# e - t
# 
# t <- proc.time()
# ld <- call_api_parallel()
# e <- proc.time()
# e - t
# 
# plot_city_hourly(layer_data)
# 
# plot_temp_map(nyc, layer)
# plot_rh_map(nyc, layer)
# 
# lon = 26:44
# lat = 24:44
# lonlat <- as.data.frame(expand.grid(lon,lat))
# colnames(lonlat) <- c("X", "Y")
# lonlat <- lonlat[!(lonlat$X <= 31 & lonlat$Y >= 33) & !(lonlat$X >= 32 & lonlat$Y <= 27),]
# write.table(lonlat, "nyc_grid_locs.csv", row.names = F, sep = ",")
