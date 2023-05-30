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
        selectInput("layer", "Select layer", c("Temperature", "Relative Humidity")),
        textAreaInput("story", "Additional Details", rows = 4)
    ),
    column(6, 
        plotOutput(outputId = "mapPlot")
    )
  ),
  
  fluidRow(
    # column(2, 
    #     textOutput("")
    # ),
    column(12, 
        plotOutput(outputId = "hourlyPlot")
    )
  )
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  getData <- reactive(
    call_api_2()
  )
  getNYCMap <- reactive(
    nyc_outline()
  )
  output$mapPlot <- renderPlot({
    # nyc <- nyc_outline()
    # layer <- call_api_2()
    if (input$layer == "Temperature"){
      plot_temp_map(getNYCMap(), getData())
    } else if (input$layer == "Relative Humidity") {
      plot_rh_map(getNYCMap(), getData())
    }
    
    
    
  })
  
  output$hourlyPlot <- renderPlot({
    plot_city_hourly(getData())
    # hist(faithful$waiting)
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)