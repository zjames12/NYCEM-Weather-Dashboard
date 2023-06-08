library(shinydashboard)
library(leaflet)
source("noaa_api.R")

header <- dashboardHeader(
  title = "Weather Dashboard",
  tags$li(a(href = 'http://www.company.com',
            img(src = 'logo1.png',
                title = "Company Home", height = "50px"),
            style = "padding-top:0px; padding-bottom:0px;"),
          class = "dropdown")
)

body <- dashboardBody(
  fluidRow(
    column(9, 
           # leafletOutput(outputId = "mapPlot")
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("mapPlot", height = 500)
           ),
    ),
    column(3, 
           box(width = NULL, status = "warning",
               selectInput("layer", "Layer", c("Temperature", "Relative Humidity", "Max. Temperature", "Min. Temperature")),
               sliderInput("num2", "Time", value = round(Sys.time(), units="hours"), min = round(Sys.time(), units="hours"),
                           max = round(Sys.time(), units="hours") + hours(6), step = 3600)
           ),
           # textAreaInput("story", "Additional Details", rows = 4)
           # sliderInput("num2", "Number two", value = round(Sys.time(), units="hours"), min = round(Sys.time(), units="hours"),
           #             max = round(Sys.time(), units="hours") + hours(10), step = 3600)
           box(width = NULL, status = "warning",
               selectInput("interval", "Refresh interval",
                           choices = c(
                             "5 minutes" = 300,
                             "10 minutes" = 600
                           ),
                           selected = "300"
               ),
               uiOutput("timeSinceLastUpdate"),
               actionButton("refresh", "Refresh now"),
               # p(class = "text-muted",
               #   br(),
               #   "Source data updates every 15 seconds."
               # )
           )
    )
    
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)