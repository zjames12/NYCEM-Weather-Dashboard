library(shiny)
library(shinydashboard)
library(leaflet)
source("noaa_api.R")


ui <- dashboardPage(
  dashboardHeader(
    title = "Weather Dashboard",
    tags$li(a(href = 'http://www.company.com',
              img(src = 'logo1.png',
                  title = "Company Home", height = "50px"),
              style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
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
             )
             # textAreaInput("story", "Additional Details", rows = 4)
             # sliderInput("num2", "Number two", value = round(Sys.time(), units="hours"), min = round(Sys.time(), units="hours"),
             #             max = round(Sys.time(), units="hours") + hours(10), step = 3600)
      )
      
    )
  )
  
)

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
shinyApp(ui = ui, server = server)