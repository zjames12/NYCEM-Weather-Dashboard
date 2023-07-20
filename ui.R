library(shinydashboard)
library(leaflet)
library(formattable)
library(shinycssloaders)
source("noaa_api.R")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

header <- dashboardHeader(
  title = "Weather Dashboard",
  tags$li(a(href = 'https://www.nyc.gov/site/em/index.page',
            img(src = 'logo1.png',
                title = "Company Home", height = "50px"),
            style = "padding-top:0px; padding-bottom:0px;"),
          class = "dropdown")
)

body <- dashboardBody(
  fluidRow(
    column(8, 
           # leafletOutput(outputId = "mapPlot")
           # img(src="sat.gif", align = "left",height='250px',width='500px'),
           box(width = NULL, solidHeader = TRUE,
               formattableOutput("centralWeather")
               # dygraphOutput("centralWeatherHourly",width = "90%", height = "110px")
           ),
           tabBox(width = NULL, height = 600,
             tabPanel("Weather Data", withSpinner(leafletOutput("mapPlot", height = 500))),
             tabPanel("NWS Products", leafletOutput("warningMap", height = 500)),
             tabPanel("Radar", imageOutput("radarImage")),
             tabPanel("Satellite", imageOutput("satImage"))
           )
           # box(width = NULL, solidHeader = TRUE,
           #     leafletOutput("mapPlot", height = 500)
           # ),
           # box(width = NULL, solidHeader = TRUE,
           #     leafletOutput("warningMap", height = 500)
           # )
    ),
    column(4, 
           box(width = NULL, height = "200px", status = "warning",
             h4("Written Analysis"),
             p (
             "Here is Josh's analysis. This section will become larger if you
             add more text. The title and valid time can be changed. The update latency
             will depend on how the final site is hosted."
             ),
             strong("Valid at 00:00")
           ),
           box(width = NULL, status = "warning",
               selectInput("layer", "Layer", c("Temperature", "Relative Humidity")),#,
                                               # "Max. Temperature", "Min. Temperature",
                                               #"Dew Point", "Heat Index")),##, "Sky Cover",
                                               # "Chance of Precipitation", "Amt. of Precipitation",
                                               # "Ice Accumulation", "Snowfall Amount", "Visibility",
                                               # "Lightning Activity", "Testing")),
               
               sliderInput("timeval", "Time", value = ceiling_date(Sys.time(), unit = "hour"), min = ceiling_date(Sys.time(), unit = "hour"),
                           max = ceiling_date(Sys.time(), unit = "hour") + hours(71), step = 3600,
                           animate = T, timezone = NULL),
               selectInput("mapType", "Map Type", c("Default", "Night", "Satellite")),
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
           ),
           box(width = NULL, status = "warning",
               h4("Watches and Warnings"),
               # p(
               #   htmlOutput("warningText")
               # )
               htmlOutput("warningText")
              )
    )
    
  ), 
  fluidRow(
    column (12, 
            box(width = NULL, solidHeader = TRUE,
                column(5, 
                       h3("Heat Index Latest Forecast"),
                       # leafletOutput("heatHazardMap")
                ),
                column(5,
                       h3("Heat Index Outlook"),
                       h6("Manhattan"),
                       dygraphOutput("boroughManattanHourly",width = "400px", height = "60px"),
                       h6("Bronx"),
                       dygraphOutput("boroughBronxHourly",width = "400px", height = "60px"),
                       h6("Brooklyn"),
                       dygraphOutput("boroughBrooklynHourly",width = "400px", height = "60px"),
                       h6("Queens"),
                       dygraphOutput("boroughQueensHourly",width = "400px", height = "60px"),
                       h6("Staten Island"),
                       dygraphOutput("boroughStatenIslandHourly",width = "400px", height = "60px"))
            
                
            )
            # tabBox(width = NULL, height = 600,
            #        tabPanel("Temp", 
            #                 column(5, 
            #                        h3("Latest Forecast"),
            #                        # leafletOutput("heatHazardMap")
            #                        ),
            #                 column(5,
            #                        h3("Heat Index Outlook"),
            #                        h6("Manhattan"),
            #                        dygraphOutput("boroughManattanHourly",width = "400px", height = "60px"),
            #                        h6("Bronx"),
            #                        dygraphOutput("boroughBronxHourly",width = "400px", height = "60px"),
            #                        h6("Brooklyn"),
            #                        dygraphOutput("boroughBrooklynHourly",width = "400px", height = "60px"),
            #                        h6("Queens"),
            #                        dygraphOutput("boroughQueensHourly",width = "400px", height = "60px"),
            #                        h6("Staten Island"),
            #                        dygraphOutput("boroughStatenIslandHourly",width = "400px", height = "60px"))
            #                 ),
            #        tabPanel("Precip", h4("Text")),
            #        tabPanel("Wind", h4("Text")),
            #        tabPanel("Coastal", h4("Text"))
      
    ),
    
    
  ),
  fluidRow(
    column (12, 
            box(width = NULL, solidHeader = TRUE,
                column(5, 
                       h3("Precipitation Latest Forecast"),
                       # leafletOutput("heatHazardMap")
                ),
                column(5,
                       h3("Precipitation Outlook")
                )
            )
    ),
  ),
  fluidRow(
    column (12, 
            box(width = NULL, solidHeader = TRUE,
                h3("Wind Forecast"),
                withSpinner(leafletOutput("windPlot", height = 500)),
                sliderInput("windtime", "Time", value = ceiling_date(Sys.time(), unit = "hour"), min = ceiling_date(Sys.time(), unit = "hour"),
                            max = ceiling_date(Sys.time(), unit = "hour") + hours(47), step = 3600,
                            animate = T, timezone = NULL)
                
            )
    ),
  ),
  fluidRow(
    column (12, 
            box(width = NULL, solidHeader = TRUE,
                h3("Coastal Hazards")
                
            )
    ),
  ),
  fluidRow(
    column (12, 
            box(width = NULL, solidHeader = TRUE,
                h3("Tropical Hazards")
            )
    ),
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)