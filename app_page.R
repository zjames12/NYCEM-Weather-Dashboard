suppressWarnings(suppressMessages(library(shiny, warn.conflicts = F)))
source("noaa_api.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  # titlePanel("Weather Dashboard"),
  # 
  # sidebarLayout(
  #   sidebarPanel(
  #     selectInput("layer", "Select layer", c("Temperature", "Relative Humidity")),
  #     textAreaInput("story", "Additional Details", rows = 4)
  #   ),
  #   mainPanel(
  #     plotOutput(outputId = "mapPlot"),
  #     plotOutput(outputId = "hourlyPlot")
  #   )
  # )
  titlePanel("Weather Dashboard"),
  
  fluidRow(
    column(6, 
        selectInput("layer", "Select layer", c("Temperature", "Relative Humidity", "Max. Temperature", "Min. Temperature")),
        textAreaInput("story", "Additional Details", rows = 4)
        # sliderInput("num2", "Number two", value = round(Sys.time(), units="hours"), min = round(Sys.time(), units="hours"),
        #             max = round(Sys.time(), units="hours") + hours(10), step = 3600)
    ),
    column(6, 
          leafletOutput(outputId = "mapPlot")
    )
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  getData <- reactive(
    call_api_2()
  )
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
  
  
  
  # output$hourlyPlot <- renderPlot({
  #   plot_city_hourly(getData())
  #   # hist(faithful$waiting)
  # })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)