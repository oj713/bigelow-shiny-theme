# Based on https://github.com/jevanilla/tenc-rshiny-workshop/blob/main/R/lessons/Section%201/11_reactive_expressions.R

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(leaflet)
})

# Import the bigelow theming materials
source("bigelow_theme.R")

s <- storms |>
  mutate(date = as.POSIXct(sprintf("%s-%s-%s %s:00", year, month, day, hour))) 

ui <- fluidPage(
  theme = bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  # Header
  bigelow_header(h2("Plotting Storms"),
                 h6("Shiny Tutorial")),
  # Main content
  bigelow_main_body(
    selectInput("storm_choice",
                "Choose a storm to plot",
                choices=sort(unique(s$name)),
                selected="Hugo"),
    checkboxInput("use_custom_icon", "Use custom icons?", value = FALSE),
    fluidRow(height = "90vh",
      column(width = 6,
             bigelow_card(headerContent = "Date vs Wind", 
                          plotOutput("wind_plot"))),
      column(width = 6,
             bigelow_card(headerContent = "Track of Storm",
                          leafletOutput("storm_track")))
    )
  ),
  # Footer with bigelow logo
  bigelow_footer("Shiny Theming Tutorial")
)

server <- function(input, output, session) {
  
  stormdata <- reactive({s |>
    filter(name == input$storm_choice) |>
    arrange(date)})
  
  output$wind_plot <- renderPlot({
    ggplot(stormdata(), aes(x=date)) + 
      geom_line(aes(y=wind)) 
  })
  
  output$storm_track <- renderLeaflet({
    data <- stormdata()
    
    addVariableMarkers <- function(leafletobj) {
      if (input$use_custom_icon) {
        custom_icon <- makeIcon(
          iconUrl = "www/images/record.png",
          iconWidth = 30,
          iconHeight = 50
        )
        addMarkers(leafletobj, ~long, ~lat, icon = custom_icon)
      } else {
        addCircleMarkers(leafletobj, ~long, ~lat, radius = 5, color = "red", fillOpacity = 0.8)
      }
    }
    
    leaflet(data = data) |>
      addTiles() |>
      addVariableMarkers()
  })
  
}

shinyApp(ui, server)
