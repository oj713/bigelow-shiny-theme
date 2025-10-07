# Lesson 1.1 - Reactive Expressions Part 2
# Objectives
# 1. Take your reactive expressions to the next level


suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(dplyr)
  library(leaflet)
})

s <- storms |>
  mutate(date = as.POSIXct(sprintf("%s-%s-%s %s:00", year, month, day, hour))) 

ui <- fluidPage(
  selectInput("storm_choice",
              "Choose a storm to plot",
              choices=sort(unique(s$name)),
              selected="Hugo"),
  fluidRow(
    column(width = 6, plotOutput("wind_plot")),
    column(width = 6, leafletOutput("storm_track"))
  )
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
    leaflet(data=stormdata()) |>
      addTiles() |>
      addCircleMarkers(~long, ~lat)
  })
  
}

shinyApp(ui, server)

# Exercises 
# 1. Make stormdata a reactive expression that updates based on the user's storm selection
# 2. Update the app to only plot one storm at a time
# Extra credit: Add a button below the storm name selection that says "Go". Make the plots only update when the button is pushed.